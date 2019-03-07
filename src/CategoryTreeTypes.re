type nodeValue = string

type treeNode = {
  value: nodeValue,
  childNodes: list(treeNode),
};