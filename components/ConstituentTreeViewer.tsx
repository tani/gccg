import Axios, { AxiosRequestConfig, AxiosResponse } from "axios";
import Graph from "dagre-d3-react";
import React, { useCallback } from "react";
import { useAsync } from "react-async";
import JsonTree from "react-json-tree";
import { v4 as uuid } from "uuid";
import { JsonRpcRequest, JsonRpcResponse } from "../lib/jsonrpc";
import { LabeledTree } from "../lib/labeled_tree";
import theme from "../lib/theme";
import { Tab, TabList, Tabs, TabPanel } from "./TabWindow";

interface ConstituentTreeViewerProps {
  text: string,
  handleTreeUpdate: (_: LabeledTree[]) => void
}

function createEdges(tree: LabeledTree): { source: string; target: string }[] {
  if ("children" in tree) {
    const edges = tree.children.map((child) => {
      return { source: JSON.stringify(tree), target: JSON.stringify(child) };
    });
    return edges.concat(...tree.children.map((child) => createEdges(child)));
  } else {
    return [];
  }
}

function createNodes(tree: LabeledTree): { id: string; label: string }[] {
  if ("children" in tree) {
    return [{ id: JSON.stringify(tree), label: tree.label.label }].concat(
      ...tree.children.map((child) => createNodes(child))
    );
  } else {
    return [{ id: JSON.stringify(tree), label: tree.label }];
  }
}

const ConstituentTreeViewer: React.FC<ConstituentTreeViewerProps> = (props) => {
  const {data, isFulfilled} = useAsync({promiseFn: useCallback(async () => {
    const config: AxiosRequestConfig = {
      headers: {
        "Content-Type": "application/json"
      }
    }
    const data: JsonRpcRequest = {
      jsonrpc: "2.0",
      id: uuid(),
      method: "process",
      params: [props.text, {
        annotators: "tokenize,ssplit,pos,lemma,ner,parse",
        "parse.binaryTrees": "true"
      }]
    }
    const response = await Axios.post<JsonRpcRequest, AxiosResponse<JsonRpcResponse<any, any>>>("./api/proxy", data, config)
    if ("error" in response.data) {
      throw Error(JSON.stringify(response.data.error))
    }
    const trees = response.data.result.sentences.map(s=>s.binaryParse) as LabeledTree[]
    props.handleTreeUpdate(trees)
    return trees
  }, [props.text])})
  return (
    <Tabs
      className="window"
      defaulTarget="visualize"
      style={{ height: "100%" }}
    >
      <div className="title-bar">
        <div className="title-bar-text">Constituent Tree</div>
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
          {isFulfilled ?  (
            data.map((tree) => {
              return (
                <Graph
                  key={JSON.stringify(tree)}
                  nodes={createNodes(tree)}
                  links={createEdges(tree)}
                  height="400"
                  fitBoundaries={true}
                  shape="circle"
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
          {isFulfilled ? <JsonTree data={data} theme={theme} /> : <></>}
        </TabPanel>
      </div>
    </Tabs>
  );
}

export default ConstituentTreeViewer;
