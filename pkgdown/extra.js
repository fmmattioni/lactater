// Sets the package name apart in running text: every plain "lactater" outside code, links and headings gets a span
// that extra.css styles.
document.addEventListener("DOMContentLoaded", () => {
  const skip = "code, pre, a, script, style, h1, h2, h3, h4, h5, h6, .lactater-name";
  const root = document.querySelector("main") || document.body;
  const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT, {
    acceptNode: (node) =>
      /\blactater\b/.test(node.nodeValue) && !node.parentElement.closest(skip)
        ? NodeFilter.FILTER_ACCEPT
        : NodeFilter.FILTER_REJECT,
  });
  const nodes = [];
  while (walker.nextNode()) nodes.push(walker.currentNode);
  for (const node of nodes) {
    const fragment = document.createDocumentFragment();
    node.nodeValue.split(/\b(lactater)\b/).forEach((part, i) => {
      if (i % 2 === 1) {
        const name = document.createElement("span");
        name.className = "lactater-name";
        name.textContent = part;
        fragment.append(name);
      } else if (part) {
        fragment.append(part);
      }
    });
    node.replaceWith(fragment);
  }
});
