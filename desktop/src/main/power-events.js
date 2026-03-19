const { trimScalar } = require("./utils");

let latestPowerEventPayload = null;
const POWER_EVENT_REPLAY_WINDOW_MS = 3 * 60 * 1000;
let ctx = null;

function buildPowerEventPayload(type, source = "desktop-main", ts = Date.now()) {
  const typeSafe = trimScalar(type, "");
  if (!typeSafe) return null;
  const sourceSafe = trimScalar(source, "desktop-main");
  const tsNum = Number.isFinite(Number(ts)) ? Number(ts) : Date.now();
  return { type: typeSafe, ts: tsNum, source: sourceSafe };
}

function emitPowerPayloadToRenderer(payload, emitKind = "direct", options = {}) {
  if (!payload || typeof payload !== "object") return Promise.resolve(false);
  const normalized = buildPowerEventPayload(payload.type, payload.source, payload.ts);
  if (!normalized) return Promise.resolve(false);
  const mainWindow = ctx.getMainWindow();
  if (!mainWindow || mainWindow.isDestroyed()) {
    ctx.appendMainLog("power_event_emit_skipped", {
      type: normalized.type,
      source: normalized.source,
      emit_kind: emitKind,
      reason: "window_unavailable"
    });
    return Promise.resolve(false);
  }

  const payloadJson = JSON.stringify(normalized);
  const waitForShinyMsRaw = Number(options.waitForShinyMs);
  const waitForShinyMs = Number.isFinite(waitForShinyMsRaw) && waitForShinyMsRaw > 0
    ? Math.round(waitForShinyMsRaw)
    : 0;
  const pollIntervalMsRaw = Number(options.pollIntervalMs);
  const pollIntervalMs = Number.isFinite(pollIntervalMsRaw) && pollIntervalMsRaw > 0
    ? Math.max(25, Math.min(1000, Math.round(pollIntervalMsRaw)))
    : 120;

  const script = waitForShinyMs > 0
    ? `(() => {
      const payload = ${payloadJson};
      const deadline = Date.now() + ${waitForShinyMs};
      const pollMs = ${pollIntervalMs};
      return new Promise((resolve) => {
        const attempt = () => {
          try {
            if (window.Shiny && typeof window.Shiny.setInputValue === "function") {
              window.Shiny.setInputValue("itcsuite_power_event", payload, { priority: "event" });
              resolve(true);
              return;
            }
          } catch (_) {}
          if (Date.now() >= deadline) {
            resolve(false);
            return;
          }
          window.setTimeout(attempt, pollMs);
        };
        attempt();
      });
    })();`
    : `(() => {
      if (window.Shiny && typeof window.Shiny.setInputValue === "function") {
        window.Shiny.setInputValue("itcsuite_power_event", ${payloadJson}, { priority: "event" });
        return true;
      }
      return false;
    })();`;

  return mainWindow.webContents.executeJavaScript(script)
    .then((ok) => {
      const delivered = ok === true;
      ctx.appendMainLog("power_event_emit", {
        type: normalized.type,
        source: normalized.source,
        emit_kind: emitKind,
        wait_for_shiny_ms: waitForShinyMs,
        delivered
      });
      return delivered;
    })
    .catch((error) => {
      ctx.appendMainLog("power_event_emit_failed", {
        type: normalized.type,
        source: normalized.source,
        emit_kind: emitKind,
        wait_for_shiny_ms: waitForShinyMs,
        error: error.message
      });
      return false;
    });
}

function emitPowerEventToRenderer(type, source = "desktop-main") {
  const payload = buildPowerEventPayload(type, source, Date.now());
  if (!payload) return Promise.resolve(false);
  latestPowerEventPayload = payload;
  return emitPowerPayloadToRenderer(payload, "direct");
}

function replayLatestPowerEventToRenderer(reason = "did-finish-load") {
  const payload = latestPowerEventPayload;
  if (!payload || typeof payload !== "object") return Promise.resolve(false);
  if (!["resume", "unlock-screen"].includes(trimScalar(payload.type, ""))) {
    return Promise.resolve(false);
  }
  const ageMs = Date.now() - Number(payload.ts || 0);
  if (!Number.isFinite(ageMs) || ageMs < 0 || ageMs > POWER_EVENT_REPLAY_WINDOW_MS) {
    return Promise.resolve(false);
  }

  const replayPayload = {
    ...payload,
    source: `${trimScalar(payload.source, "desktop-main")}.replay`
  };
  return emitPowerPayloadToRenderer(replayPayload, "replay", {
    waitForShinyMs: 10000,
    pollIntervalMs: 120
  });
}

function setupPowerEvents(context) {
  ctx = context;
  const { powerMonitor } = require("electron");

  if (!ctx.smokeMode) {
    powerMonitor.on("suspend", () => {
      ctx.appendMainLog("power_suspend", {});
      emitPowerEventToRenderer("suspend", "power-monitor");
    });
    powerMonitor.on("resume", () => {
      ctx.appendMainLog("power_resume", {});
      emitPowerEventToRenderer("resume", "power-monitor");
      if (ctx.recoverAfterResume) {
        setTimeout(() => ctx.recoverAfterResume("power-resume"), 2000);
      }
    });
    powerMonitor.on("unlock-screen", () => {
      ctx.appendMainLog("unlock_screen", {});
      emitPowerEventToRenderer("unlock-screen", "power-monitor");
      if (ctx.recoverAfterResume) {
        setTimeout(() => ctx.recoverAfterResume("unlock-screen"), 2000);
      }
    });
  }
}

module.exports = {
  setupPowerEvents,
  emitPowerEventToRenderer,
  replayLatestPowerEventToRenderer
};
