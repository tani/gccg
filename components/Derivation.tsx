import dynamic from "next/dynamic";
import React from "react";
import { useSelector } from "react-redux";
import { LabeledLabelTree, LabeledTree } from "../lib/labeled_tree";
import { RootState } from "../lib/slice";
import { TabWindow } from "./TabWindow";
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
  const derivations = useSelector<RootState, LabeledTree[][]>((state) => state.derivations);
  return (
    <TabWindow title="Derivation" data={derivations}>
      {derivations.map((derivations, index) => {
        const src = derivations
          .map((derivation) => "\\begin{prooftree}" + renderMathJaxDerivation(derivation) + "\\end{prooftree}")
          .join("\\\\");
        const options = { display: true };
        return (
          <div key={src}>
            <p>Sentence {index}</p>
            <MathJax src={`\\displaylines{${src}}`} options={options} />
          </div>
        );
      })}
    </TabWindow>
  );
};

export default Window;
