import React, { useEffect, useState } from "react";
import theme from "../lib/theme";
import { LabeledTree, LabeledLabelTree } from "../lib/labeled_tree";
import JsonTree from "react-json-tree";
import MathJax from "./MathJax";
import NumericInput from "react-numeric-input";
import hash from "object-hash";
import { Tab, TabList, Tabs, TabPanel } from "./TabWindow";

function renderMathJaxSrcCategory(tree: LabeledLabelTree): string {
  if ("children" in tree) {
    switch (tree.label) {
      case "left":
        return `(${renderMathJaxSrcCategory(
          tree.children[0]
        )}/${renderMathJaxSrcCategory(tree.children[1])})`;
      case "right":
        return `(${renderMathJaxSrcCategory(
          tree.children[0]
        )}\\backslash ${renderMathJaxSrcCategory(tree.children[1])})`;
    }
  } else {
    return tree.label;
  }
}

function renderGrammar(grammar: LabeledTree[]) {
  const body = grammar
    .map((rule) => {
      if ("children" in rule && rule.children.length === 1) {
        const word = rule.children[0].label;
        const category = renderMathJaxSrcCategory(rule.label);
        return `${word} &\\rightarrow ${category}`;
      } else {
        throw new Error(`Malformed Grammar: ${rule}`);
      }
    })
    .join("\\\\");
  return `\\begin{align*}${body}\\end{align*}`;
}

interface GrammarViewerProps {
  handleGrammarSelection(grammar: LabeledTree[][]): void;
  grammars?: LabeledTree[][][];
}

const GrammarPreviewer: React.FC<{
  grammars: LabeledTree[][];
  handleGrammarChange: (g: LabeledTree[]) => void;
}> = (props) => {
  const [i, setState] = useState(0);
  return (
    <>
      <NumericInput
        min={0}
        max={props.grammars.length - 1}
        value={i}
        onChange={(v) => {
          props.handleGrammarChange(props.grammars[v]);
          setState(v);
        }}
      />
      <MathJax
        src={renderGrammar(props.grammars[i])}
        options={{ display: true }}
      />
    </>
  );
};

const GrammarView: React.FC<GrammarViewerProps> = (props) => {
  const [state, setState] = useState(props.grammars?.map((g) => g[0]));
  useEffect(()=>{
    setState(props.grammars?.map((g)=>g[0]))
  }, [props.grammars])
  return (
    <Tabs
      className="window"
      defaulTarget="visualize"
      style={{ height: "100%" }}
    >
      <div className="title-bar">
        <div className="title-bar-text">Grammar</div>
      </div>
      <div className="window-body" style={{ height: "100%" }}>
        <TabList role="tablist">
          <Tab role="tab" target="visualize">
            Visualize
          </Tab>
          <Tab role="tab" target="json">
            JSON
          </Tab>
        </TabList>
        <TabPanel
          role="tabpanel"
          title="visualize"
          style={{ height: "calc(100% - 60px)", overflow: "auto" }}
        >
          {props.grammars ? (
            props.grammars.map((grammars, k) => {
              const handleGrammarChange = (grammar) => {
                props.handleGrammarSelection(
                  state.map((g, i) => (i === k ? grammar : g))
                );
                setState(state.map((g, i) => (i === k ? grammar : g)));
              };
              return (
                <GrammarPreviewer
                  key={hash(grammars)}
                  grammars={grammars}
                  handleGrammarChange={handleGrammarChange}
                />
              );
            })
          ) : (
            <p>Waiting input or response...</p>
          )}
        </TabPanel>
        <TabPanel
          role="tabpanel"
          title="json"
          style={{ height: "calc(100% - 60px)", overflow: "auto" }}
        >
          <JsonTree data={props.grammars} theme={theme} />
        </TabPanel>
      </div>
    </Tabs>
  );
};

export default GrammarView;
