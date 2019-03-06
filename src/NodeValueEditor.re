type state = string;

type action =
  | Change(string);

let component = ReasonReact.reducerComponent("NodeValueEditor");

let make = (~value, ~onChange, _children) => {
  let blur = (_event, self) => onChange(self.ReasonReact.state);
  let keyDown = (event, self) =>
    event->ReactEvent.Keyboard.key == "Enter" ?
      onChange(self.ReasonReact.state) : ();
  {
    ...component,

    initialState: () => value,

    reducer: (action, _state) =>
      switch (action) {
      | Change(value) => ReasonReact.Update(value)
      },

    render: self =>
      <input
        value={self.state}
        autoFocus=true
        onChange={event =>
          event->ReactEvent.Form.target##value->Change->(self.send)
        }
        onBlur={self.handle(blur)}
        onKeyDown={self.handle(keyDown)}
      />,
  };
};