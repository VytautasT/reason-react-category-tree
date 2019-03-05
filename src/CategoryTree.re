type treeNode = {
  value: string,
  childNodes: array(treeNode),
};

type state = {
  tree: treeNode
};

let component = ReasonReact.reducerComponent("CategoryTree");

let rec renderChildNodes = (self, childNodes) =>
  ReasonReact.array(Js.Array.map(renderNode(self), childNodes))
and renderNode = (self, {value, childNodes}) => 
  <div style={ReactDOMRe.Style.make(~marginLeft="10px", ())}>
    {ReasonReact.string("- ")}
    {ReasonReact.string(value)}
    {renderChildNodes(self, childNodes)}
  </div>;

let renderTree = self => {
  <div>
    {ReasonReact.string(self.ReasonReact.state.tree.value)}
    {renderChildNodes(self, self.state.tree.childNodes)}
  </div>;
};

let make = _children => {
  ...component,

  initialState: () => {
    tree: {
      value: "root",
      childNodes: [|
        {
          value: "foo",
          childNodes: [|
            {value: "foobar", childNodes: [||]},
            {value: "foofoo", childNodes: [||]},
          |],
        },
        {
          value: "bar",
          childNodes: [|
            {value: "barbar", childNodes: [||]},
            {value: "barfoo", childNodes: [||]},
          |],
        },
      |],
    }
  },

  reducer: ((), _state: state) => ReasonReact.NoUpdate,

  render: self => renderTree(self),
};