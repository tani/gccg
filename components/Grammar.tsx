import dynamic from "next/dynamic";
import React, { useState } from "react";
import JsonTree from "react-json-tree";
import { useDispatch, useSelector } from "react-redux";
import { LabeledTree, LabeledLabelTree } from "../lib/labeled_tree";
import { constructDerivation, RootState } from "../lib/slice";
import theme from "../lib/theme";
import { Tab, TabList, Tabs, TabPanel } from "./TabWindow";

const MathJax = dynamic(() => import("./MathJax"), { ssr: false });

function renderMathJaxCategory(tree: LabeledLabelTree): string {
  if ("children" in tree) {
    const category1 = renderMathJaxCategory(tree.children[0]);
    const category2 = renderMathJaxCategory(tree.children[1]);
    switch (tree.label) {
      case "left":
        return `(${category1} \\backslash  ${category2})`;
      case "right":
        return `(${category1} / ${category2})`;
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
        const category = renderMathJaxCategory(rule.label);
        return `${word} &\\rightarrow ${category}`;
      } else {
        throw new Error(`Malformed Grammar: ${rule}`);
      }
    })
    .join("\\\\");
  return `\\begin{align*}${body}\\end{align*}`;
}

const Selector: React.FC<{ index: number }> = (props) => {
  const [state, setState] = useState(0);
  const allGrammars = useSelector<RootState, LabeledTree[][][]>(state=>state.allGrammars)
  const dispatch = useDispatch();

  const handleClick: React.ChangeEventHandler<HTMLInputElement> = (event) => {
    const index = parseInt(event.target.value)
    dispatch(constructDerivation({ grammar: allGrammars[props.index][index], index: props.index }));
    setState(index);
  };
  return (
    <>
      <input type="number" max={allGrammars[props.index].length - 1} min={0} value={state} onChange={handleClick} />
      <MathJax src={renderGrammar(allGrammars[props.index][state])} options={{ display: true }} />
    </>
  );
};

const Grammar: React.FC = () => {
  const allGrammars = useSelector<RootState, LabeledTree[][][]>((state)=>state.allGrammars)
  return (
    <Tabs className="window" defaulTarget="visualize">
      <div className="title-bar">
        <div className="title-bar-text">Grammar</div>
      </div>
      <div className="window-body">
        <TabList role="tablist">
          <Tab role="tab" target="visualize">
            Visualize
          </Tab>
          <Tab role="tab" target="json">
            JSON
          </Tab>
        </TabList>
        <TabPanel role="tabpanel" title="visualize">
          {allGrammars.map((g, index) => (
            <Selector key={JSON.stringify(g)} index={index} />
          ))}
        </TabPanel>
        <TabPanel role="tabpanel" title="json">
          <JsonTree data={allGrammars} theme={theme} />
        </TabPanel>
      </div>
    </Tabs>
  );
};

export default Grammar;
