import React, { useState } from "react";

interface SentenceFormProps {
  handleSentenceUpdate(sentence: string): void;
}

const SentenceForm: React.FC<SentenceFormProps> = (props) =>{
  const [sentence, setSentence] = useState("");
  return (
    <div className="window" style={{height:"100%"}}>
      <div className="title-bar">
        <div className="title-bar-text">Sentence</div>
      </div>
      <div className="window-body">
        <form
          onSubmit={(event) => {
            props.handleSentenceUpdate(sentence);
            event.preventDefault();
          }}
        >
          <textarea
            onChange={(event) => setSentence(event.target.value)}
            style={{ width: "100%" }}
          />
          <button type="submit">Submit</button>
        </form>
      </div>
    </div>
  );
}

export default SentenceForm
