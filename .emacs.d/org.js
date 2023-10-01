// modify some html to make code blocks distinguishable from output blocks
// output blocks have a child with class '.language-text' (theyre basicly text code blocks)
window.onload = function() {
  prettifyCodeBlocks();
}

function wrap (toWrap, wrapper) {
    wrapper = wrapper || document.createElement('div');
    toWrap.parentNode.insertBefore(wrapper, toWrap);
    return wrapper.appendChild(toWrap);
};

function getTextBlocks() {
  return document.getElementsByClassName('language-text');
}

function prettifyCodeBlocks() {
  for (let elt of getTextBlocks()) {
    let codeBlock = elt.parentElement.parentElement; // codeBlock has class .highlight
    console.log(codeBlock);
    codeBlock.classList.add('output');
    // let wrapper = document.createElement('div');
    // wrapper.classList.add('output-block-wrapper');
    // wrap(codeBlock, wrapper);
  }
}