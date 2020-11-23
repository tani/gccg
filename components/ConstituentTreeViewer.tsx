import React from "react";
import theme from "../lib/theme";
import { LabeledTree } from "../lib/labeled_tree";
import hash from "object-hash";
import JsonTree from "react-json-tree";
import Graph from "dagre-d3-react";
import { Tab, TabList, Tabs, TabPanel } from "./TabWindow";

interface ConstituentTreeViewerProps {
  constituentTrees?: LabeledTree[];
}

function createEdges(tree: LabeledTree): { source: string; target: string }[] {
  if ("children" in tree) {
    const edges = tree.children.map((child) => {
      return { source: hash(tree), target: hash(child) };
    });
    return edges.concat(...tree.children.map((child) => createEdges(child)));
  } else {
    return [];
  }
}

function createNodes(tree: LabeledTree): { id: string; label: string }[] {
  if ("children" in tree) {
    return [{ id: hash(tree), label: tree.label.label }].concat(
      ...tree.children.map((child) => createNodes(child))
    );
  } else {
    return [{ id: hash(tree), label: tree.label }];
  }
}

const ConstituentTreeViewer: React.FC<ConstituentTreeViewerProps> = (props) => {
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
          {props.constituentTrees ? (
            props.constituentTrees.map((tree) => {
              return (
                <Graph
                  key={hash(tree)}
                  nodes={createNodes(tree)}
                  links={createEdges(tree)}
                  height="400"
                  fitBoundaries={true}
                  shape="circle"
                />
              );
            })
          ) : (
            <p>Waiting input or response ...</p>
          )}
        </TabPanel>
        <TabPanel
          role="tabpanel"
          title="json"
          style={{ height: "calc(100% - 95px)", overflow: "auto" }}
        >
          <JsonTree data={props.constituentTrees} theme={theme} />
        </TabPanel>
      </div>
    </Tabs>
  );
}

export default ConstituentTreeViewer;
