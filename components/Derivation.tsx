import dynamic from "next/dynamic";
import React, { useState } from "react";
import JsonTree from "react-json-tree";
import { useSelector } from "react-redux";
import { LabeledLabelTree, LabeledTree } from "../lib/labeled_tree";
import { RootState } from "../lib/slice";
import theme from "../lib/theme";
import { Tab, Tabs, TabList, TabPanel } from "./TabWindow";
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

function renderMathJaxDerivation(tree: LabeledTree): string {
  if ("children" in tree) {
    if (tree.children.length === 2) {
      const axiom1 = renderMathJaxDerivation(tree.children[0]);
      const axiom2 = renderMathJaxDerivation(tree.children[1]);
      const conclusion = renderMathJaxCategory(tree.label);
      return axiom1 + axiom2 + `\\BIC{$${conclusion}$}`;
    } else if (tree.children.length === 1) {
      const axiom1 = renderMathJaxDerivation(tree.children[0]);
      const conclusion = renderMathJaxCategory(tree.label);
      return axiom1 + `\\UIC{$${conclusion}$}`;
    }
  } else {
    return `\\AXC{$${tree.label}$}`;
  }
}

const Window: React.FC = () => {
  const derivations = useSelector<RootState, LabeledTree[][]>((state)=>state.derivations)
  const [state, setState] = useState("medium")
  return (
    <Tabs className="window" defaulTarget="visualize">
      <div className="title-bar">
        <div className="title-bar-text">CCG Derivations</div>
      </div>
      <div className="window-body" style={{fontSize: state}}>
        <TabList role="tablist">
          <Tab role="tab" target="visualize">
            Visualize
          </Tab>
          <Tab role="tab" target="json">
            JSON
          </Tab>
          <Tab role="tab" target="configuration">
            Configuration
          </Tab>
        </TabList>
        <TabPanel role="tabpanel" title="visualize">
          {derivations.map((derivations, index) => {
            const src = derivations.map(derivation=>"\\begin{prooftree}" + renderMathJaxDerivation(derivation) + "\\end{prooftree}").join("\\\\")
            const options = { display: true };
            return (
              <div key={src}>
              <p>Sentence {index}</p>
              <MathJax src={`\\displaylines{${src}}`} options={options} />
              </div>
            )
          })}
        </TabPanel>
        <TabPanel role="tabpanel" title="json">
          <JsonTree data={derivations} theme={theme} />
        </TabPanel>
        <TabPanel role="tabpanel" title="configuration">
          <label htmlFor="font-size" style={{ marginRight: 10 }}>Font size</label>
          <select name="font-size" onChange={(e)=>{setState(e.target.value)}}>
            {["xx-small", "x-small", "small", "medium", "large", "x-large", "xx-large"].map(scale=>(
              <option key={scale} value={scale}>{scale}</option>
            ))}
          </select>
        </TabPanel>
      </div>
    </Tabs>
  );
};

export default Window;
