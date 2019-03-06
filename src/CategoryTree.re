type treeNode = {
  value: string,
  childNodes: list(treeNode),
};

type dirtyNode = {
  node: treeNode,
  uncommitedValue: string,
};

type state = {
  root: treeNode,
  dirtyNode: option(dirtyNode),
  hoveredNode: option(treeNode),
};

type action =
  | Add(list(treeNode))
  | Remove(list(treeNode))
  | EnterEditMode(treeNode)
  | Edit(string)
  | ExitEditMode(list(treeNode))
  | MouseEnter(treeNode)
  | MouseLeave(treeNode);

let component = ReasonReact.reducerComponent("CategoryTree");

let renderInput = (~value, ~handleChange, ~handleBlur) =>
  <input
    value
    autoFocus=true
    onChange={event => event->ReactEvent.Form.target##value->handleChange}
    onBlur={_event => handleBlur()}
    onKeyDown={event =>
      event->ReactEvent.Keyboard.key == "Enter" ? handleBlur() : ()
    }
  />;

let renderButtons = (~self, ~ancestors) =>
  <>
    <button onClick={_event => self.ReasonReact.send(Add(ancestors))}>
      {ReasonReact.string("+")}
    </button>
    {List.length(ancestors) > 0 ?
       <button onClick={_event => self.send(Remove(ancestors))}>
         {ReasonReact.string("x")}
       </button> :
       ReasonReact.null}
  </>;

let rec renderNode = (~self, ~node, ~ancestors, ~index=?, ()) => {
  let {root, dirtyNode, hoveredNode} = self.ReasonReact.state;
  let handleChange = value => value->Edit->(self.send);
  let handleBlur = () => ancestors->ExitEditMode->(self.send);
  <div
    key=?index
    style={ReactDOMRe.Style.make(~marginLeft="10px", ())}
    onMouseEnter={_event => node->MouseEnter->(self.send)}
    onMouseLeave={_event => node->MouseLeave->(self.send)}>
    {root == node ? ReasonReact.null : ReasonReact.string("- ")}
    {switch (dirtyNode) {
     | Some(dn) when dn.node == node =>
       renderInput(~value=dn.uncommitedValue, ~handleChange, ~handleBlur)
     | _ =>
       <span onDoubleClick={_event => node->EnterEditMode->(self.send)}>
         {ReasonReact.string(node.value)}
       </span>
     }}
    {switch (hoveredNode) {
     | Some(hn) when hn == node => renderButtons(~self, ~ancestors)
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
          List.map(child => child == oldChild ? newChild : child)
        | _ => List.filter(child => child != oldChild)
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
  ReasonReact.Update({
    ...state,
    dirtyNode: Some({node, uncommitedValue: node.value}),
  });

let setUncommitedValue = (state, value) =>
  ReasonReact.Update({
    ...state,
    dirtyNode:
      Belt.Option.map(state.dirtyNode, ({node}) =>
        {node, uncommitedValue: value}
      ),
  });

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
    dirtyNode: Some({node: newNode, uncommitedValue: newNode.value}),
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
        hn == node ? Some(hn) : None
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
    | (Edit(value), _) => setUncommitedValue(state, value)
    | (ExitEditMode(ancestors), {dirtyNode: Some({node, uncommitedValue})})
        when uncommitedValue != "" && uncommitedValue != node.value =>
      commitValue(state, node, uncommitedValue, ancestors)
    | (Remove(ancestors), {hoveredNode: Some(node)}) =>
      removeNode(state, node, ancestors)
    | (Add(ancestors), {hoveredNode: Some(node)}) =>
      addNode(state, node, ancestors)
    | (ExitEditMode(_), _) => clearDirtyNode(state)
    | (MouseEnter(node), _) => setHoveredNode(state, node)
    | (MouseLeave(node), _) => clearHoveredNodeIfUnchanged(state, node)
    | _ => ReasonReact.NoUpdate
    },
  render: self => renderNode(~self, ~ancestors=[], ~node=self.state.root, ()),
};