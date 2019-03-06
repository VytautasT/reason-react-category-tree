let initialTreeData: CategoryTree.treeNode = {
  value: "root",
  childNodes: [
    {
      value: "foo",
      childNodes: [
        {value: "foobar", childNodes: []},
        {value: "foofoo", childNodes: []},
      ],
    },
    {
      value: "bar",
      childNodes: [
        {value: "barbar", childNodes: []},
        {value: "barfoo", childNodes: []},
      ],
    },
  ],
};