import React, { useState } from "react";
import { useDispatch } from "react-redux";
import { generateTrees } from "../lib/slice";

const Sentence: React.FC = () => {
  const dispatch = useDispatch()
  const [state, setState] = useState("");
  const handleSubmit: React.ChangeEventHandler<HTMLFormElement> = (event) => {
    dispatch(generateTrees(state))
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
