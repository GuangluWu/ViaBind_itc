const { app } = require("electron");
const { spawn, spawnSync } = require("child_process");
const path = require("path");
const fs = require("fs");
const EventEmitter = require("events");
const os = require("os");

const READY_PREFIX = "ITCSUITE_READY ";
const ERROR_PREFIX = "ITCSUITE_ERROR ";

function useBundledRuntimeInDev() {
  return process.env.ITCSUITE_USE_BUNDLED_R === "1";
}

class BackendController extends EventEmitter {
  constructor(options) {
    super();
    this.options = options;
    this.state = "stopped";
    this.child = null;
    this.stdoutBuffer = "";
    this.readyTimeout = null;
    this.readyPort = null;
  }

  transition(nextState) {
    this.state = nextState;
  }

  resolvePaths() {
    if (app.isPackaged) {
      const repoRoot = path.join(process.resourcesPath, "itcsuite");
      const launchScript = path.join(repoRoot, "ITCSuiteWeb", "scripts", "launch_shiny.R");
      const runtimeRoot = path.join(process.resourcesPath, "r-runtime");
      return { repoRoot, launchScript, runtimeRoot };
    }

    const repoRoot = process.env.ITCSUITE_REPO_ROOT || path.resolve(__dirname, "../../..");
    const launchScript = path.join(repoRoot, "ITCSuiteWeb", "scripts", "launch_shiny.R");
    const runtimeRoot = process.env.ITCSUITE_RUNTIME_ROOT || path.join(repoRoot, "desktop", "resources", "r-runtime");
    return { repoRoot, launchScript, runtimeRoot };
  }

  async resolveRscript(runtimeRoot) {
    if (process.env.ITCSUITE_RSCRIPT) {
      return {
        path: process.env.ITCSUITE_RSCRIPT,
        kind: "override",
        label: "env override"
      };
    }

    // In development we prefer system Rscript to avoid accidentally
    // using a partially built bundled runtime.
    if (!app.isPackaged && !useBundledRuntimeInDev()) {
      return {
        path: "Rscript",
        kind: "system",
        label: "PATH Rscript"
      };
    }

    const candidates = [
      ...((app.isPackaged || useBundledRuntimeInDev()) ? [{
        path: path.join(runtimeRoot, "bin", "itcsuite-rscript"),
        kind: "bundled",
        label: "bundled launcher"
      }] : []),
      {
        path: path.join(runtimeRoot, "bin", "Rscript.exe"),
        kind: "bundled",
        label: "bundled Rscript.exe"
      },
      {
        path: path.join(runtimeRoot, "bin", "x64", "Rscript.exe"),
        kind: "bundled",
        label: "bundled x64/Rscript.exe"
      },
      {
        path: path.join(runtimeRoot, "bin", "Rscript"),
        kind: "bundled",
        label: "bundled bin/Rscript"
      },
      {
        path: path.join(runtimeRoot, "Resources", "bin", "Rscript"),
        kind: "bundled",
        label: "bundled Resources/bin/Rscript"
      },
      ...(process.platform === "darwin" ? [{
        path: "/Library/Frameworks/R.framework/Resources/bin/Rscript",
        kind: "system",
        label: "system framework Rscript"
      }] : [])
    ];

    const resolvedCandidates = candidates.filter((candidate) => {
      if (candidate.path === "Rscript") return true;
      return fs.existsSync(candidate.path);
    });

    if (app.isPackaged) {
      for (const candidate of resolvedCandidates) {
        const probe = await this.probeRscriptCandidate(candidate, runtimeRoot);
        this.appendLog(
          `[${new Date().toISOString()}] R_PROBE ${candidate.label} (${candidate.path}) => ${probe.ok ? "OK" : "FAIL"}${probe.reason ? `: ${probe.reason}` : ""}\n`
        );
        if (probe.ok) {
          return candidate;
        }
      }
    }

    for (const candidate of resolvedCandidates) {
      return candidate;
    }

    if (app.isPackaged) {
      throw new Error(`No usable Rscript found under ${runtimeRoot}. The bundled R framework may be missing or corrupt.`);
    }

    return {
      path: "Rscript",
      kind: "system",
      label: "PATH Rscript"
    };
  }

  buildEnv(runtimeRoot, rscriptPath = "", rscriptKind = "bundled") {
    const env = { ...process.env };
    env.ITCSUITE_DESKTOP = "1";
    env.ITCSUITE_USER_DATA_DIR = app.getPath("userData");
    env.ITCSUITE_APP_VERSION = app.getVersion();
    if (typeof rscriptPath === "string" && rscriptPath.trim()) {
      env.ITCSUITE_RSCRIPT = rscriptPath;
    }

    const bundledLib = path.join(runtimeRoot, "library");
    if (fs.existsSync(bundledLib) && (app.isPackaged || useBundledRuntimeInDev())) {
      const isolatedUserLib = path.join(env.ITCSUITE_USER_DATA_DIR || os.tmpdir(), "r-library-empty");
      try {
        fs.mkdirSync(isolatedUserLib, { recursive: true });
      } catch (_) {
        // Best effort only.
      }
      env.ITCSUITE_RUNTIME_ROOT = runtimeRoot;
      env.R_LIBS = bundledLib;
      env.R_LIBS_SITE = bundledLib;
      env.R_LIBS_USER = isolatedUserLib;
      if (rscriptKind !== "system") {
        env.R_HOME = runtimeRoot;
      }

      const runtimeBins = [
        path.join(runtimeRoot, "bin", "x64"),
        path.join(runtimeRoot, "bin"),
        path.join(runtimeRoot, "Resources", "bin")
      ].filter((candidate) => fs.existsSync(candidate));

      if (runtimeBins.length > 0) {
        const currentPath = env.PATH || env.Path || "";
        const prefixedPath = runtimeBins.join(path.delimiter);
        env.PATH = currentPath ? `${prefixedPath}${path.delimiter}${currentPath}` : prefixedPath;
        if (Object.prototype.hasOwnProperty.call(env, "Path")) {
          env.Path = env.PATH;
        }
      }
    }

    // Preserve Unicode rendering in bundled runtime on Unix-like hosts.
    if (process.platform !== "win32") {
      if (!env.LANG || !/UTF-?8/i.test(env.LANG)) {
        env.LANG = "en_US.UTF-8";
      }
      if (!env.LC_CTYPE || !/UTF-?8/i.test(env.LC_CTYPE)) {
        env.LC_CTYPE = "en_US.UTF-8";
      }
    }

    return env;
  }

  probeRscriptCandidate(candidate, runtimeRoot) {
    return new Promise((resolve) => {
      const probeScript = [
        "cat('ITCSUITE_PROBE_R_HOME=', normalizePath(R.home(), winslash='/', mustWork=FALSE), '\\n', sep='')",
        "for (lib in normalizePath(.libPaths(), winslash='/', mustWork=FALSE)) cat('ITCSUITE_PROBE_LIB=', lib, '\\n', sep='')"
      ].join(";");
      const env = this.buildEnv(runtimeRoot, candidate.path, candidate.kind);

      const child = spawn(candidate.path, ["-e", probeScript], {
        cwd: runtimeRoot,
        env,
        windowsHide: true,
        stdio: ["ignore", "pipe", "pipe"]
      });

      let stdout = "";
      let stderr = "";

      child.stdout.on("data", (chunk) => { stdout += chunk.toString("utf8"); });
      child.stderr.on("data", (chunk) => { stderr += chunk.toString("utf8"); });

      let done = false;
      const timeout = setTimeout(() => {
        if (!done) {
          done = true;
          try { child.kill(); } catch (_) {}
          resolve({ ok: false, reason: "probe timeout (15000ms)" });
        }
      }, 15000);

      const finish = (error, status, signal) => {
        if (done) return;
        done = true;
        clearTimeout(timeout);

        const failed = error || signal || status !== 0;
        if (failed && process.platform === "win32" && app.isPackaged) {
          // Fallback simple --version execution if complex probe fails on Windows
          const fallback = spawnSync(candidate.path, ["--version"], {
            cwd: runtimeRoot, env, encoding: "utf8", timeout: 5000, windowsHide: true
          });
          if (fallback.status === 0 && !fallback.error && !fallback.signal) {
            resolve({ ok: true, reason: "Fallback --version passed" });
            return;
          }
        }

        if (error) {
          resolve({ ok: false, reason: error.message });
          return;
        }
        if (signal) {
          resolve({ ok: false, reason: `terminated by signal ${signal}` });
          return;
        }
        if (status !== 0) {
          const trimmedStderr = stderr.trim();
          resolve({ ok: false, reason: `exit ${status}${trimmedStderr ? `; ${trimmedStderr}` : ""}` });
          return;
        }

        const rHomeMatch = stdout.match(/^ITCSUITE_PROBE_R_HOME=(.+)$/m);
        const libMatches = [...stdout.matchAll(/^ITCSUITE_PROBE_LIB=(.+)$/gm)].map((match) => match[1]);
        const bundledLib = path.join(runtimeRoot, "library").replace(/\\/g, "/");
        const normalizedRuntimeRoot = runtimeRoot.replace(/\\/g, "/");

        if (libMatches.length > 0 && !libMatches.includes(bundledLib)) {
          resolve({ ok: false, reason: `bundled library missing from .libPaths(): ${bundledLib}` });
          return;
        }

        if (candidate.kind === "bundled" && process.platform === "darwin") {
          const rHome = rHomeMatch ? rHomeMatch[1] : "";
          if (!rHome.startsWith(normalizedRuntimeRoot)) {
            resolve({ ok: false, reason: `R.home() resolved outside bundled runtime: ${rHome || "<empty>"}` });
            return;
          }
        }

        resolve({ ok: true, reason: rHomeMatch ? `R.home=${rHomeMatch[1]}` : "" });
      };

      child.on("error", (err) => finish(err, null, null));
      child.on("close", (code, signal) => finish(null, code, signal));
    });
  }

  processLine(line) {
    const trimmed = line.trim();
    if (!trimmed) return;

    if (trimmed.startsWith(READY_PREFIX)) {
      const payloadRaw = trimmed.slice(READY_PREFIX.length);
      try {
        const payload = JSON.parse(payloadRaw);
        const port = Number(payload.port);
        if (!Number.isFinite(port) || port <= 0) {
          throw new Error("invalid ready payload");
        }
        this.readyPort = port;
        clearTimeout(this.readyTimeout);
        this.transition("ready");
        this.emit("backend-ready", port);
      } catch (error) {
        this.emit("backend-error", `Malformed READY payload: ${error.message}`);
      }
      return;
    }

    if (trimmed.startsWith(ERROR_PREFIX)) {
      const payloadRaw = trimmed.slice(ERROR_PREFIX.length);
      let message = payloadRaw;
      try {
        const payload = JSON.parse(payloadRaw);
        message = payload.message || payloadRaw;
      } catch (_) {
        // Keep plain message fallback.
      }
      this.emit("backend-error", message);
    }
  }

  appendLog(line) {
    try {
      fs.appendFileSync(this.options.backendLogPath, line);
    } catch (_) {
      // Best effort logging.
    }
  }

  async start() {
    if (this.state !== "stopped") return;

    try {
      this.transition("resolving");
      const { repoRoot, launchScript, runtimeRoot } = this.resolvePaths();

      if (!fs.existsSync(launchScript)) {
        throw new Error(`launch_shiny.R missing: ${launchScript}`);
      }

      const rscriptCandidate = await this.resolveRscript(runtimeRoot);
      const rscript = rscriptCandidate.path;
      const args = [
        launchScript,
        "--repo-root", repoRoot,
        "--app-dir", "ITCSuiteWeb",
        "--host", this.options.host,
        "--port", "0",
        "--log-dir", this.options.logsDir
      ];

      this.transition("booting");
      this.readyPort = null;
      this.stdoutBuffer = "";

      this.appendLog(`\n[${new Date().toISOString()}] START ${rscript} ${args.join(" ")}\n`);

      this.child = spawn(rscript, args, {
        cwd: repoRoot,
        env: this.buildEnv(runtimeRoot, rscript, rscriptCandidate.kind),
        stdio: ["ignore", "pipe", "pipe"]
      });

      this.child.stdout.on("data", (chunk) => {
        const text = chunk.toString("utf8");
        this.appendLog(text);
        this.stdoutBuffer += text;

        let idx = this.stdoutBuffer.indexOf("\n");
        while (idx >= 0) {
          const line = this.stdoutBuffer.slice(0, idx);
          this.processLine(line);
          this.stdoutBuffer = this.stdoutBuffer.slice(idx + 1);
          idx = this.stdoutBuffer.indexOf("\n");
        }
      });

      this.child.stderr.on("data", (chunk) => {
        const text = chunk.toString("utf8");
        this.appendLog(text);
      });

      this.child.on("error", (error) => {
        this.emit("backend-error", `Failed to start backend: ${error.message}`);
      });

      this.child.on("close", (code) => {
        clearTimeout(this.readyTimeout);
        const exitCode = Number.isFinite(code) ? code : -1;
        const wasReady = this.state === "ready";
        const wasStopping = this.state === "stopping";
        this.child = null;
        this.transition("stopped");
        this.emit("backend-exit", exitCode, wasReady, wasStopping);
      });

      this.readyTimeout = setTimeout(() => {
        if (this.state === "booting") {
          this.emit("backend-error", "Timed out waiting for backend READY payload.");
          this.stop();
        }
      }, 90000);
    } catch (error) {
      this.transition("stopped");
      this.emit("backend-error", error.message);
    }
  }

  async stop() {
    if (!this.child) {
      this.transition("stopped");
      return;
    }

    this.transition("stopping");
    const child = this.child;

    await new Promise((resolve) => {
      let settled = false;
      const finish = () => {
        if (!settled) {
          settled = true;
          resolve();
        }
      };

      // On Windows, SIGTERM has no effect on child processes.
      // Skip the graceful SIGTERM phase and kill directly.
      const useDirectKill = process.platform === "win32";
      const gracePeriodMs = useDirectKill ? 0 : 5000;

      const timeout = setTimeout(() => {
        if (!child.killed) {
          try {
            child.kill("SIGKILL");
          } catch (_) {
            // Ignore kill failures.
          }
        }
      }, gracePeriodMs);

      child.once("close", () => {
        clearTimeout(timeout);
        finish();
      });

      try {
        if (useDirectKill) {
          // Windows: kill immediately (TerminateProcess).
          child.kill();
        } else {
          // Unix: send SIGTERM for graceful shutdown.
          child.kill("SIGTERM");
        }
      } catch (_) {
        clearTimeout(timeout);
        finish();
      }
    });
  }

  async restart() {
    await this.stop();
    await this.start();
  }
}

module.exports = BackendController;
