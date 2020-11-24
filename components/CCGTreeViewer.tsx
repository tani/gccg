import React, { useCallback } from "react";
import { LabeledLabelTree, LabeledTree } from "../lib/labeled_tree";
import theme from "../lib/theme";
import JsonTree from "react-json-tree";
import hash from "object-hash";
import request from "../lib/create_request";
import { Tab, Tabs, TabList, TabPanel } from "./TabWindow";
import Async from "react-async";
import dynamic from "next/dynamic"
const MathJax = dynamic(()=>import("./MathJax"), { ssr: false });
interface CCGTreeViewerProps {
  grammar: LabeledTree[];
}

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

function RendeMathJaxSrcDerivation(tree: LabeledTree): string {
  if ("children" in tree) {
    if (tree.children.length === 2) {
      return `${RendeMathJaxSrcDerivation(
        tree.children[0]
      )}${RendeMathJaxSrcDerivation(
        tree.children[1]
      )}\\BIC{$${renderMathJaxSrcCategory(tree.label)}$}`;
    } else if (tree.children.length === 1) {
      return `${RendeMathJaxSrcDerivation(
        tree.children[0]
      )}\\UIC{$${renderMathJaxSrcCategory(tree.label)}$}`;
    }
  } else {
    return `\\AXC{$${tree.label}$}`;
  }
}

const CCGXTreeViewer: React.FC<{
  grammars: LabeledTree[][];
}> = (props) => {
  return (
    <Tabs
      className="window"
      defaulTarget="visualize"
      style={{ height: "100%" }}
    >
      <div className="title-bar">
        <div className="title-bar-text">CCG Derivations</div>
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
          style={{ height: "calc(100% - 95px)", overflow: "auto" }}
        >
          {props.grammars ? (
            props.grammars.map((grammar, i) => (
              <div key={hash(grammar)}>
                <p>Sentence {i}</p>
                <CCGTreeViewer grammar={grammar} />
                <hr></hr>
              </div>
            ))
          ) : (
            <p>Waiting input or response...</p>
          )}
        </TabPanel>
        <TabPanel
          role="tabpanel"
          title="json"
          style={{ height: "calc(100% - 95px)", overflow: "auto" }}
        >
          <JsonTree data={props.grammars} theme={theme} />
        </TabPanel>
      </div>
    </Tabs>
  );
};

export default CCGXTreeViewer;

const CCGTreeViewer: React.FC<CCGTreeViewerProps> = (props) => {
  return (
    <Async
      promiseFn={useCallback(() => request("construct", props.grammar), [
        props.grammar,
      ])}
    >
      <Async.Pending>
        <p>Pending...</p>
      </Async.Pending>
      <Async.Rejected>{(error) => <p>{error.message}</p>}</Async.Rejected>
      <Async.Fulfilled>
        {(data) =>
          (data as LabeledTree[]).map((derivation) => {
            return (
              <MathJax
                key={hash(derivation)}
                src={`\\begin{prooftree}${RendeMathJaxSrcDerivation(
                  derivation
                )}\\end{prooftree}`}
                options={{
                  display: true,
                }}
              />
            );
          })
        }
      </Async.Fulfilled>
    </Async>
  );
};
