import dynamic from "next/dynamic";
import React, { useContext, useState } from "react";
import JsonTree from "react-json-tree";
import NumericInput from "react-numeric-input";
import { LabeledTree, LabeledLabelTree } from "../lib/labeled_tree";
import { AppContext } from "../lib/store";
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
  const context = useContext(AppContext);
  const handleClick = (index: number) => {
    context.dispatch({
      type: "ConstructDerivationRequest",
      index,
      grammar: context.allGrammars[props.index][index],
    });
    setState(index);
  };
  return (
    <>
      <NumericInput min={0} max={context.allGrammars[props.index].length - 1} value={state} onChange={handleClick} />
      <MathJax src={renderGrammar(context.allGrammars[props.index][state])} options={{ display: true }} />
    </>
  );
};

const Grammar: React.FC = () => {
  const context = useContext(AppContext);

  const JsonPanel = <JsonTree data={context.allGrammars} theme={theme} />;

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
          {context.allGrammars.map((g, index) => (
            <Selector key={JSON.stringify(g)} index={index} />
          ))}
        </TabPanel>
        <TabPanel role="tabpanel" title="json">
          {JsonPanel}
        </TabPanel>
      </div>
    </Tabs>
  );
};

export default Grammar;
