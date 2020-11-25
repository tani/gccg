import React, { useContext, useState } from "react";
import { AppContext } from "../lib/store";

const Sentence: React.FC = () => {
  const context = useContext(AppContext);
  const [state, setState] = useState("");
  const handleSubmit: React.ChangeEventHandler<HTMLFormElement> = (event) => {
    context.dispatch({
      type: "GenerateTreesRequest",
      text: state
    })
    event.preventDefault();
  };
  const handleChange: React.ChangeEventHandler<HTMLTextAreaElement> = (event) => {
    setState(event.target.value);
  };
  return (
    <div className="window">
      <div className="title-bar">
        <div className="title-bar-text">Sentence</div>
      </div>
      <div className="window-body">
        <form onSubmit={handleSubmit}>
          <textarea onChange={handleChange}/>
          <button type="submit">Submit</button>
        </form>
      </div>
    </div>
  );
};

export default Sentence;
