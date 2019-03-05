let component = ReasonReact.statelessComponent("CategoryTree");

let make = _children => {
  ...component,
  render: _self =>
    <div>
      {ReasonReact.string("Category tree")}
    </div>
};
