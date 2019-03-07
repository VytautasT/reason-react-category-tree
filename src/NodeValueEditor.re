type inputValue = CategoryTreeTypes.nodeValue;

type state = {
  value: inputValue,
  inputRef: ref(option(Dom.element)),
};

type action =
  | Change(inputValue);

let component = ReasonReact.reducerComponent("NodeValueEditor");

let make = (~value, ~onChange, _children) => {
  let change = (event, self) =>
    event->ReactEvent.Form.target##value->Change->(self.ReasonReact.send);
  let blur = (_event, self) => onChange(self.ReasonReact.state.value);
  let keyDown = (event, self) =>
    event->ReactEvent.Keyboard.key == "Enter" ?
      onChange(self.ReasonReact.state.value) : ();
  let setInputRef = (theRef, self) => {
    self.ReasonReact.state.inputRef := Js.Nullable.toOption(theRef);
  };
  {
    ...component,

    initialState: () => {value, inputRef: ref(None)},

    didMount: ({state}) =>
      switch (state.inputRef^) {
      | None => ()
      | Some(r) => ReactDOMRe.domElementToObj(r)##focus() /* I solemnly swear that I am up to no good */
      },

    reducer: (action, state) =>
      switch (action) {
      | Change(value) => ReasonReact.Update({...state, value})
      },

    render: self =>
      <input
        value={self.state.value}
        onChange={self.handle(change)}
        onBlur={self.handle(blur)}
        onKeyDown={self.handle(keyDown)}
        ref={self.handle(setInputRef)}
      />,
  };
};