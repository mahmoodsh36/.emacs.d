// modify some html to make code blocks distinguishable from output blocks
// output blocks have a child with class '.language-text' (theyre basically text code blocks)
window.addEventListener("load", function () {
  prettifyCodeBlocks();
  let customBlocks = [
      'lemma',
      'proof',
      'entail',
      "entailment",
      "definition",
      "note",
      "my_example",
      "characteristic",
      "assumption",
      "question",
      "subquestion",
      "answer",
      "solution",
      "proposition",
      "problem",
      "subproblem",
      "step",
      "code-output",
      "code",
      "axiom",
      "thought",
  ];
  for (let cb of customBlocks) {
    let elements = document.getElementsByClassName(cb);
    for (let element of elements) {
//      console.log(elements);
//      if (element.classList.contains('custom-block'))
//        continue;
      let wrapper = document.createElement('div');
      wrapper.classList.add('custom-block');
      if (cb == 'my_example') {
        wrapper.setAttribute('data-before-content', 'example');
      } else if (cb == 'code-output') {
        wrapper.setAttribute('data-before-content', 'code output');
      } else {
        wrapper.setAttribute('data-before-content', cb);
      }
      wrap(element, wrapper);

      /*let title = element.getAttribute('data-title');
      if (title) {
        // console.log(title);
      }*/
    }
  }
});

function wrap(toWrap, wrapper) {
    wrapper = wrapper || document.createElement('div');
    toWrap.parentNode.insertBefore(wrapper, toWrap);
    return wrapper.appendChild(toWrap);
}

function getTextBlocks() {
  // let elts = document.getElementsByClassName('example');
  // return elts;
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