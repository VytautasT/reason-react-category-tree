open CategoryTreeTypes;

type state = {
  root: treeNode,
  dirtyNode: option(treeNode),
  hoveredNode: option(treeNode),
};

type action =
  | Add(list(treeNode))
  | Remove(list(treeNode))
  | EnterEditMode(treeNode)
  | ExitEditMode(nodeValue, list(treeNode))
  | MouseEnter(treeNode)
  | MouseLeave(treeNode);

let component = ReasonReact.reducerComponent("CategoryTree");

let renderButtons = (~self, ~ancestors) =>
  <>
    <button onClick={_event => Add(ancestors)->(self.ReasonReact.send)}>
      {ReasonReact.string("+")}
    </button>
    {List.length(ancestors) > 0 ?
       <button onClick={_event => Remove(ancestors)->(self.send)}>
         {ReasonReact.string("x")}
       </button> :
       ReasonReact.null}
  </>;

let rec renderNode = (~self, ~node, ~ancestors, ~index=?, ()) => {
  let {root, dirtyNode, hoveredNode} = self.ReasonReact.state;
  let handleEditorChange = value =>
    ExitEditMode(value, ancestors)->(self.send);
  <div
    key=?index
    style={ReactDOMRe.Style.make(~marginLeft="10px", ())}
    onMouseEnter={_event => MouseEnter(node)->(self.send)}
    onMouseLeave={_event => MouseLeave(node)->(self.send)}>
    {root === node ? ReasonReact.null : ReasonReact.string("- ")}
    {switch (dirtyNode) {
     | Some(dn) when dn === node =>
       <NodeValueEditor value={node.value} onChange=handleEditorChange />
     | _ =>
       <span onDoubleClick={_event => EnterEditMode(node)->(self.send)}>
         {ReasonReact.string(node.value)}
       </span>
     }}
    {switch (hoveredNode) {
     | Some(hn) when hn === node => renderButtons(~self, ~ancestors)
     | _ => ReasonReact.null
     }}
    {node.childNodes
     |> List.mapi((index, child) =>
          renderNode(
            ~self,
            ~ancestors=[node, ...ancestors],
            ~node=child,
            ~index=string_of_int(index),
            (),
          )
        )
     |> Array.of_list
     |> ReasonReact.array}
  </div>;
};

let updateNode =
  List.fold_left(((oldChild, newChildOption), parent) => {
    let childNodes =
      (
        switch (newChildOption) {
        | Some(newChild) =>
          List.map(child => child === oldChild ? newChild : child)
        | _ => List.filter(child => child !== oldChild)
        }
      )(
        parent.childNodes,
      );
    let newParent = {...parent, childNodes};
    (parent, Some(newParent));
  });

let updateRoot = (root, oldNode, newNode, ancestors) =>
  ancestors
  |> updateNode((oldNode, newNode))
  |> snd
  |> Belt_Option.getWithDefault(_, root);

let setDirtyNode = (state, node) =>
  ReasonReact.Update({...state, dirtyNode: Some(node)});

let commitValue = (state, node, value, ancestors) =>
  ReasonReact.Update({
    hoveredNode: None,
    dirtyNode: None,
    root: updateRoot(state.root, node, Some({...node, value}), ancestors),
  });

let removeNode = (state, node, ancestors) =>
  ReasonReact.Update({
    hoveredNode: None,
    dirtyNode: None,
    root: updateRoot(state.root, node, None, ancestors),
  });

let addNode = (state, node, ancestors) => {
  let newNode = {value: "new", childNodes: []};
  ReasonReact.Update({
    hoveredNode: None,
    dirtyNode: Some(newNode),
    root:
      updateRoot(
        state.root,
        node,
        Some({...node, childNodes: [newNode, ...node.childNodes]}),
        ancestors,
      ),
  });
};

let setHoveredNode = (state, node) =>
  ReasonReact.Update({...state, hoveredNode: Some(node)});

let clearDirtyNode = state => ReasonReact.Update({...state, dirtyNode: None});

let clearHoveredNodeIfUnchanged = (state, node) =>
  ReasonReact.Update({
    ...state,
    hoveredNode:
      Belt_Option.flatMap(state.hoveredNode, hn =>
        hn === node ? Some(hn) : None
      ),
  });

let make = (~initialTreeData, _children) => {
  ...component,

  initialState: () => {
    root: initialTreeData,
    dirtyNode: None,
    hoveredNode: None,
  },

  reducer: (action, state) =>
    switch (action, state) {
    | (EnterEditMode(node), _) => setDirtyNode(state, node)
    | (ExitEditMode(value, ancestors), {dirtyNode: Some(node)})
        when value != "" && value != node.value =>
      commitValue(state, node, value, ancestors)
    | (ExitEditMode(_), _) => clearDirtyNode(state)
    | (Remove(ancestors), {hoveredNode: Some(node)}) =>
      removeNode(state, node, ancestors)
    | (Add(ancestors), {hoveredNode: Some(node)}) =>
      addNode(state, node, ancestors)
    | (MouseEnter(node), _) => setHoveredNode(state, node)
    | (MouseLeave(node), _) => clearHoveredNodeIfUnchanged(state, node)
    | _ => ReasonReact.NoUpdate
    },
  render: self => renderNode(~self, ~ancestors=[], ~node=self.state.root, ()),
};