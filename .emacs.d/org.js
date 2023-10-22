// modify some html to make code blocks distinguishable from output blocks
// output blocks have a child with class '.language-text' (theyre basicly text code blocks)
window.addEventListener("load", function () {
  prettifyCodeBlocks();
  let customBlocks = [
      'lemma',
      'proof',
      'entail',
      "lemma",
      "proof",
      "entailment",
      "definition",
      "note",
      "my_example",
      "characteristic",
      "assumption",
      "question",
      "subquestion",
      "answer",
      "step",
      "code-output",
      "code",
  ];
  for (let cb of customBlocks) {
    for (let element of document.getElementsByClassName(cb)) {
      if (cb == 'my_example') {
        element.setAttribute('data-before-content', 'example');
      } else if (cb == 'code-output') {
        element.setAttribute('data-before-content', 'code output');
      } else {
        element.setAttribute('data-before-content', cb);
      }
    }
  }
});

function wrap(toWrap, wrapper) {
    wrapper = wrapper || document.createElement('div');
    toWrap.parentNode.insertBefore(wrapper, toWrap);
    return wrapper.appendChild(toWrap);
}

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
    textBlock.classList.remove('code');
    textBlock.classList.add('code-output');
    // let wrapper = document.createElement('div');
    // wrapper.classList.add('output-block-wrapper');
    // wrap(codeBlock, wrapper);
  }
}