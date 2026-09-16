/* Real DOM regressions with synthetic traffic; never contacts a live agent. */
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const {chromium} = require('playwright');
async function run() {
  const browser = await chromium.launch({headless:true, executablePath:process.env.PAI_TEST_BROWSER});
  let checks = 0;
  try {
    const context = await browser.newContext({viewport:{width:393,height:852}});
    await context.addInitScript(() => {
      window.EventSource = class { constructor(){ window.stream=this; } close(){} };
    });
    let writable = true, uploadFails = false, saved = '', marker = '', allowOlder;
    const olderGate = new Promise(resolve => { allowOlder = resolve; });
    await context.route('**/*', async route => {
      const url = new URL(route.request().url()), request = route.request();
      const files = {'/terminal':'terminal.html','/settings':'settings.html','/app-shell.js':'app-shell.js','/viewport.js':'viewport.js','/settings.js':'settings.js'};
      const reply = (data,status=200) => route.fulfill({status,contentType:'application/json',body:JSON.stringify(data)});
      if (files[url.pathname]) return route.fulfill({contentType:url.pathname.endsWith('.js')?'text/javascript':'text/html',body:fs.readFileSync(path.join(__dirname,'../src/adapters/web/assets',files[url.pathname]),'utf8')});
      if (url.pathname === '/api/v2/history') {
        if (url.searchParams.has('before')) {
          await olderGate;
          return reply({events:Array.from({length:20},(_,i)=>({id:i+1,type:'final',data:'Older history '+i})),has_more:false});
        }
        return reply({events:Array.from({length:100},(_,i)=>({id:i+100,type:'final',data:'Message '+i+'\n'+('An older message to read. '.repeat(8))})),has_more:true});
      }
      if (url.pathname === '/api/v2/files') return reply({root:'/fixture',can_write:writable,entries:[{name:'note.txt',is_dir:false},{name:'binary.bin',is_dir:false}]});
      if (url.pathname === '/api/v2/file') {
        if (request.method()==='POST') { saved=request.postDataJSON().content; marker=request.headers()['x-pai-request']; return reply({ok:true}); }
        const binary = url.searchParams.get('path') === 'binary.bin';
        return reply({content:binary?'':'File contents',editable:!binary,can_write:writable});
      }
      if (url.pathname === '/api/v2/upload') return reply(uploadFails?{error:'Upload rejected'}:{ok:true},uploadFails?400:200);
      if (url.pathname === '/api/settings/runtime') {
        if(request.method()==='POST') marker=request.headers()['x-pai-request'];
        return reply({revision:1,settings:[{key:'web_file_mutation',type:'boolean',desired:true,effective:true,description:'Uploads',apply_mode:'restart'}]});
      }
      return reply({});
    });
    const page = await context.newPage(), errors=[];
    page.on('pageerror',error=>errors.push(error.message));
    await page.goto('http://ui.test/terminal');
    await page.waitForFunction(()=>window.stream && document.querySelectorAll('.ev').length===100);
    const bottom = () => page.evaluate(()=>{const t=document.querySelector('#transcript'); return t.scrollHeight-t.clientHeight-t.scrollTop;});
    assert.ok(await bottom()<2); checks++;
    await page.locator('#transcript').evaluate(el=>el.scrollTop=600);
    await page.waitForTimeout(80);
    const anchor = await page.evaluate(()=>captureReadingPosition().node.textContent);
    const offset = await page.evaluate(()=>captureReadingPosition().offset);
    for (const event of [
      {type:'ambient',data:'private work'},
      {type:'operational',data:{message:'An operational notice'}},
      {type:'stream-progress',data:{generation_id:'private',private:true,display:'Private generation'}},
      {type:'stream-progress',data:{generation_id:'public',private:false,display:'Public generation'}},
      {type:'final',data:'A new response'}]) {
      await page.evaluate(event=>window.stream.onmessage({data:JSON.stringify({id:300,...event})}),event);
      await page.waitForTimeout(80);
      assert.equal(await page.evaluate(()=>captureReadingPosition().node.textContent),anchor);
      assert.ok(Math.abs(await page.evaluate(()=>captureReadingPosition().offset)-offset)<2); checks+=2;
    }
    await page.evaluate(()=>{const row=document.querySelector('.ev'); row.style.height='700px';});
    await page.waitForTimeout(80);
    assert.equal(await page.evaluate(()=>captureReadingPosition().node.textContent),anchor); checks++;
    // A delayed history response must preserve where the reader moved during the request.
    await page.evaluate(()=>document.querySelector('#loadmore').click());
    await page.locator('#transcript').evaluate(el=>el.scrollTop=1200);
    await page.waitForTimeout(80);
    const moved = await page.evaluate(()=>({text:captureReadingPosition().node.textContent,offset:captureReadingPosition().offset}));
    allowOlder();
    await page.waitForFunction(()=>!document.querySelector('#loadmore').disabled);
    assert.equal(await page.evaluate(()=>captureReadingPosition().node.textContent),moved.text);
    assert.ok(Math.abs(await page.evaluate(()=>captureReadingPosition().offset)-moved.offset)<2); checks+=2;
    await page.waitForTimeout(2600);
    assert.equal(await page.evaluate(()=>captureReadingPosition().node.textContent),moved.text); checks++;
    await page.getByRole('button',{name:'Jump to latest'}).click();
    await page.evaluate(()=>window.stream.onmessage({data:JSON.stringify({id:301,type:'final',data:'Another response'})}));
    await page.waitForTimeout(80); assert.ok(await bottom()<2); checks++;
    await page.getByRole('button',{name:'Open navigation menu'}).click();
    await page.getByRole('button',{name:'Files',exact:true}).click();
    await page.locator('.entry').filter({hasText:'note.txt'}).click();
    await page.waitForFunction(()=>document.querySelector('#fm-textarea').value==='File contents');
    await page.getByRole('textbox',{name:'File contents'}).fill('Changed text');
    await page.locator('#fm-save').click(); await page.waitForFunction(()=>document.querySelector('#fm-status').textContent==='Saved');
    assert.equal(saved,'Changed text'); assert.equal(marker,'same-origin'); checks+=2;
    await page.locator('.entry').filter({hasText:'binary.bin'}).click();
    await page.waitForFunction(()=>document.querySelector('#fm-status').textContent.includes('Binary'));
    assert.ok(await page.locator('#fm-save').isDisabled()); assert.ok(await page.getByText('Download',{exact:true}).isVisible()); checks+=2;
    await page.locator('#fm-upload-input').setInputFiles({name:'upload.txt',mimeType:'text/plain',buffer:Buffer.from('upload')});
    uploadFails=true; await page.locator('#fm-upload-btn').click();
    await page.waitForFunction(()=>document.querySelector('#fm-status').textContent==='Upload rejected');
    assert.ok(await page.locator('#fm-upload-btn').isEnabled()); checks++;
    uploadFails=false; await page.locator('#fm-upload-btn').click();
    await page.waitForFunction(()=>document.querySelector('#fm-status').textContent==='Uploaded'); checks++;
    writable=false; await page.evaluate(()=>fmNavigate(''));
    assert.ok(await page.locator('#fm-upload-btn').isDisabled()); checks++;
    await page.goto('http://ui.test/settings');
    await page.getByRole('button',{name:'Increase text size'}).click();
    assert.equal(await page.locator('#font-size').textContent(),'15 px');
    assert.equal(await page.evaluate(()=>document.body.classList.contains('pai-login')),false); checks+=2;
    await page.locator('#settings select').selectOption('false');
    await page.locator('#settings button').click();
    await page.waitForFunction(()=>document.querySelector('#status').textContent.includes('durably saved'));
    assert.equal(marker,'same-origin'); checks++;
    await page.goto('http://ui.test/terminal');
    await page.waitForFunction(()=>document.querySelector('.ev.final'));
    assert.equal(await page.locator('.ev.final').first().evaluate(el=>getComputedStyle(el).fontSize),'15px'); checks++;
    if (process.env.PAI_TEST_SCREENSHOTS) {
      fs.mkdirSync(process.env.PAI_TEST_SCREENSHOTS,{recursive:true});
      await page.screenshot({path:path.join(process.env.PAI_TEST_SCREENSHOTS,'terminal-ui.png')});
      await page.getByRole('button',{name:'Open navigation menu'}).click();
      await page.getByRole('button',{name:'Files',exact:true}).click();
      await page.locator('.entry').filter({hasText:'note.txt'}).click();
      await page.waitForFunction(()=>document.querySelector('#fm-textarea').value==='File contents');
      await page.screenshot({path:path.join(process.env.PAI_TEST_SCREENSHOTS,'files-ui.png')});
    }
    assert.deepEqual(errors,[]); checks++;
    console.log(`Terminal UI: ${checks} checks passed.`);
  } finally { await browser.close(); }
}
run().catch(error=>{console.error(error);process.exitCode=1;});
