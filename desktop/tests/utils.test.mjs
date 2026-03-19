import assert from "node:assert/strict";
import { createRequire } from "node:module";

const require = createRequire(import.meta.url);
const { trimScalar, escapeHtml } = require("../src/main/utils.js");

function testTrimScalar() {
  assert.equal(trimScalar("  hello  "), "hello");
  assert.equal(trimScalar("", "fallback"), "fallback");
  assert.equal(trimScalar(null, "default"), "default");
  assert.equal(trimScalar(undefined, "default"), "default");
  assert.equal(trimScalar(42), "42");
  assert.equal(trimScalar("  "), "");
  assert.equal(trimScalar("  ", "x"), "x");
  assert.equal(trimScalar("text"), "text");
}

function testEscapeHtmlBasicEntities() {
  assert.equal(escapeHtml("<script>alert('xss')</script>"),
    "&lt;script&gt;alert(&#039;xss&#039;)&lt;/script&gt;");
  assert.equal(escapeHtml('He said "hello"'), "He said &quot;hello&quot;");
  assert.equal(escapeHtml("a & b"), "a &amp; b");
  assert.equal(escapeHtml("no special chars"), "no special chars");
}

function testEscapeHtmlEdgeCases() {
  assert.equal(escapeHtml(""), "");
  assert.equal(escapeHtml(null), "");
  assert.equal(escapeHtml(undefined), "");
  assert.equal(escapeHtml(42), "42");
  assert.equal(escapeHtml("<b>bold</b>"), "&lt;b&gt;bold&lt;/b&gt;");
}

function testEscapeHtmlNestedEntities() {
  // Ensure & in already-escaped text gets double-escaped (no unescaping)
  assert.equal(escapeHtml("&amp;"), "&amp;amp;");
  assert.equal(escapeHtml("&lt;"), "&amp;lt;");
}

function testEscapeHtmlPathLikeStrings() {
  // File paths that might appear in error messages
  assert.equal(escapeHtml("/Users/alice/Library/Application Support"),
    "/Users/alice/Library/Application Support");
  assert.equal(
    escapeHtml("C:\\Users\\bob\\AppData\\Local"),
    "C:\\Users\\bob\\AppData\\Local"
  );
}

function testEscapeHtmlMultilineErrorMessage() {
  const errorMessage = `Error at line 5:
    Expected <integer> but got "string"
    In file: /tmp/test & verify.R`;
  const escaped = escapeHtml(errorMessage);
  assert.ok(!escaped.includes("<integer>"));
  assert.ok(escaped.includes("&lt;integer&gt;"));
  assert.ok(escaped.includes("&amp;"));
  assert.ok(escaped.includes("&quot;string&quot;"));
}

function run() {
  testTrimScalar();
  testEscapeHtmlBasicEntities();
  testEscapeHtmlEdgeCases();
  testEscapeHtmlNestedEntities();
  testEscapeHtmlPathLikeStrings();
  testEscapeHtmlMultilineErrorMessage();
}

run();
console.log("utils.test.mjs: OK");
