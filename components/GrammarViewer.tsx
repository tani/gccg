import Axios, { AxiosRequestConfig, AxiosResponse } from "axios";
import { produce } from "immer";
import dynamic from "next/dynamic"
import React, { useCallback, useState } from "react";
import { useAsync } from "react-async";
import JsonTree from "react-json-tree";
import NumericInput from "react-numeric-input";
import { v4 } from "uuid";
import { JsonRpcRequest, JsonRpcResponse } from "../lib/jsonrpc";
import { LabeledTree, LabeledLabelTree } from "../lib/labeled_tree";
import theme from "../lib/theme";
import { Tab, TabList, Tabs, TabPanel } from "./TabWindow";

const MathJax = dynamic(()=>import("./MathJax"), { ssr: false });

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
  trees: LabeledTree[];
}

const GrammarPreviewer: React.FC<{
  nGrammar: number,
  grammar: LabeledTree[];
  handleGrammarChange: (k: number) => void;
}> = (props) => {
  const [i, setState] = useState(0);
  return (
    <>
      <NumericInput
        min={0}
        max={props.nGrammar - 1}
        value={i}
        onChange={(v) => {
          props.handleGrammarChange(v);
          setState(v);
        }}
      />
      <MathJax
        src={renderGrammar(props.grammar)}
        options={{ display: true }}
      />
    </>
  );
};

const GrammarView: React.FC<GrammarViewerProps> = (props) => {
  const [state, setState] = useState<LabeledTree[][]>([]);
  const { data, isFulfilled } = useAsync({
    promiseFn: useCallback(async () => {
      const listOfGrammars = await Promise.all(
        props.trees.map(async (tree) => {
          const config: AxiosRequestConfig = {
            headers: { "Content-Type": "application/json" },
          };
          const data: JsonRpcRequest = {
            jsonrpc: "2.0",
            id: v4(),
            method: "extract",
            params: [tree],
          };
          const response = await Axios.post<JsonRpcRequest, AxiosResponse<JsonRpcResponse<any, any>>>(
            "./api/proxy",
            data,
            config
          );
          if ("error" in response.data) {
            throw Error(JSON.stringify(response.data.error));
          }
          const grammars = response.data.result as LabeledTree[][];
          return grammars;
        })
      );
      props.handleGrammarSelection(listOfGrammars.map(g=>g[0]))
      setState(listOfGrammars.map(g=>g[0]))
      return listOfGrammars;
    }, [props.trees]),
  });
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
          style={{ height: "calc(100% - 95px)", overflow: "auto" }}
        >
          { isFulfilled ? (
            data.map((grammars, k) => {
              const handleGrammarChange = (i: number) => {
                const nextState = produce(state, (draftState) => {
                  draftState[k] = grammars[i];
                });
                props.handleGrammarSelection(nextState);
                setState(nextState);
              };
              return (
                <GrammarPreviewer
                  key={JSON.stringify(data[k])}
                  nGrammar={grammars.length}
                  grammar={state[k] || []}
                  handleGrammarChange={handleGrammarChange}
                />
              );
            })
          ) : <></>}
        </TabPanel>
        <TabPanel
          role="tabpanel"
          title="json"
          style={{ height: "calc(100% - 95px)", overflow: "auto" }}
        >
          {isFulfilled ? (
            <JsonTree data={data} theme={theme} />
          ) : <></>}
        </TabPanel>
      </div>
    </Tabs>
  );
};

export default GrammarView;
