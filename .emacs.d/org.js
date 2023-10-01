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
  let elts = document.getElementsByClassName('language-text');
  let textBlocks = [];
  for (let elt of elts) {
    textBlocks.push(elt.parentElement.parentElement);
  }
  return textBlocks;
}

function getCodeBlocks() {
  return document.getElementsByClassName('highlight');
}

function prettifyCodeBlocks() {
  // do code blocks before text blocks cuz text blocks are a subset of them so doing them later overrides the properties set for code blocks
  for (let codeBlock of getCodeBlocks()) {
    codeBlock.classList.add('code');
  }
  for (let textBlock of getTextBlocks()) {
    console.log(textBlock);
    textBlock.classList.remove('code');
    textBlock.classList.add('code-output');
    // let wrapper = document.createElement('div');
    // wrapper.classList.add('output-block-wrapper');
    // wrap(codeBlock, wrapper);
  }
}