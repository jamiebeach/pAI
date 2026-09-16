/* Geometry regressions in a real browser; physical iOS acceptance is separate.
 * Run with NODE_PATH pointing to an installed Playwright, no package download.
 */
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const {keyboardGeometry} = require('../src/adapters/web/assets/viewport.js');
const vv = {height:450, scale:1, offsetTop:8};
assert.deepEqual(keyboardGeometry(852, vv, true), {height:450,top:8});
assert.equal(keyboardGeometry(852, vv, false), null);
assert.equal(keyboardGeometry(852, {...vv,scale:2}, true), null);
assert.equal(keyboardGeometry(852, {...vv,height:852}, true), null);
assert.equal(keyboardGeometry(852, null, true), null);

async function run() {
  const {chromium} = require('playwright');
  const browser = await chromium.launch({headless:true, ...(process.env.PAI_TEST_BROWSER ? {executablePath:process.env.PAI_TEST_BROWSER} : {})});
  let checks = 5;
  try {
    for (const standalone of [true,false]) {
      const context = await browser.newContext({viewport:{width:393,height:852}});
      await context.addInitScript(standalone => {
        Object.defineProperty(navigator, 'standalone', {value:standalone});
        // An oversized screen must not become either app height or padding.
        Object.defineProperty(screen, 'height', {value:1000});
        const fake = new EventTarget();
        Object.assign(fake,{height:852,width:393,scale:1,offsetTop:0,offsetLeft:0});
        Object.defineProperty(window,'visualViewport',{value:fake, configurable:true});
      }, standalone);
      await context.route('**/*', route => {
        const url = new URL(route.request().url());
        const file = {'/terminal':'terminal.html','/login':'login.html','/viewport.js':'viewport.js'}[url.pathname];
        if (file) return route.fulfill({status:200, contentType:file.endsWith('.js')?'text/javascript':'text/html',
          body:fs.readFileSync(path.join(__dirname,'../src/adapters/web/assets',file),'utf8')});
        return route.fulfill({status:200,contentType:'application/json',body:'{"events":[],"files":[]}'});
      });
      const page = await context.newPage();
      for (const entry of ['/terminal','/login']) {
        await page.goto('http://viewport.test'+entry);
        await page.waitForFunction(() => window.paiViewport);
        const geometry = await page.evaluate(() => window.paiViewport.snapshot());
        assert.equal(geometry.standalone,standalone);
        assert.equal(geometry.shell.height,852);
        assert.equal(geometry.shell.bottom,852);
        assert.ok(parseFloat(geometry.shell.paddingBottom) < 50);
        if (geometry.send) assert.ok(geometry.send.bottom <=852);
        checks += 5;
        if (process.env.PAI_TEST_SCREENSHOTS && standalone) {
          fs.mkdirSync(process.env.PAI_TEST_SCREENSHOTS,{recursive:true});
          await page.screenshot({path:path.join(process.env.PAI_TEST_SCREENSHOTS,entry.slice(1)+'.png')});
        }
        await page.evaluate(() => window.paiViewport.showDiagnostics());
        await page.waitForFunction(() => document.querySelector('#pai-layout-panel pre').textContent.includes('visual-viewport-v2'));
        assert.ok(!(await page.locator('#pai-layout-panel').innerText()).includes('password'));
        await page.getByRole('button',{name:'Close layout diagnostics'}).click();
        // Test actual event bindings, not a second implementation of the policy.
        await page.evaluate(() => {
          Object.assign(window.visualViewport,{height:450,offsetTop:8});
          document.querySelector('#input, input[name="username"]').focus();
          window.visualViewport.dispatchEvent(new Event('resize'));
        });
        await page.waitForFunction(() => document.documentElement.classList.contains('pai-keyboard'));
        assert.equal((await page.evaluate(() => window.paiViewport.snapshot())).shell.bottom,458);
        await page.evaluate(() => { Object.defineProperty(window,'innerHeight',{value:450,configurable:true}); document.activeElement.blur(); window.visualViewport.dispatchEvent(new Event('scroll')); });
        await page.waitForTimeout(50);
        assert.equal((await page.evaluate(() => window.paiViewport.snapshot())).shell.bottom,458);
        checks++;
        if (entry === '/terminal') {
          await page.evaluate(() => {
            document.querySelector('.modal').classList.add('open');
            document.querySelector('#fm-textarea').focus();
            window.dispatchEvent(new Event('resize'));
          });
          await page.waitForFunction(() => window.paiViewport.snapshot().files?.height ===450);
          assert.equal((await page.evaluate(() => window.paiViewport.snapshot())).files.bottom,458);
          await page.evaluate(() => document.querySelector('.modal').classList.remove('open'));
          checks++;
        }
        await page.evaluate(() => { window.visualViewport.scale=2; window.dispatchEvent(new Event('resize')); });
        await page.waitForFunction(() => !document.documentElement.style.getPropertyValue('--pai-visible-height'));
        assert.equal((await page.evaluate(() => window.paiViewport.snapshot())).shell.height,852);
        await page.setViewportSize({width:852,height:393});
        await page.evaluate(() => { document.activeElement.blur(); window.dispatchEvent(new Event('pageshow')); });
        await page.waitForFunction(() => window.paiViewport.snapshot().shell.height ===393);
        await page.evaluate(() => document.dispatchEvent(new Event('visibilitychange')));
        await page.waitForFunction(() => window.paiViewport.snapshot().reason === 'visible-resume');
        checks += 4;
        await page.setViewportSize({width:393,height:852});
      }
      await context.close();
    }
    console.log(`Viewport: ${checks} checks passed; physical iOS gate remains open.`);
  } finally { await browser.close(); }
}
run().catch(error => { console.error(error); process.exitCode=1; });
