
const csoundjs = "./csound.js";
let csound = null;
let orc = null;

fetch("../orc/acerbumdulci-jubilo.orc")
    .then(response => response.text())
    .then(orctext => {
	console.log(orctext);
	orc = orctext;
    });

async function Start() {
    if(csound == null) {
	console.log('starting csound')	
	const { Csound } = await import(csoundjs);
	csound = await Csound();
	await csound.setOption("-odac");
	await csound.compileOrc(orc);
	await csound.start();
    } else {
	console.log('already started csound')
    }
}

// async function ring(scorecount, shade) {
//     console.log('input score event ' + scorecount + ' ' + shade);
//     let scoreevent = 'i3 0 1 ' + scorecount + ' ' + shade; 
//     await csound.inputMessage(scoreevent);
// }

// window.startCsound = startCsound;
// window.ring = ring;
