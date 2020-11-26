import Graph from "dagre-d3-react";
import React from "react";
import JsonTree from "react-json-tree";
import { useSelector } from "react-redux";
import { LabeledTree } from "../lib/labeled_tree";
import { RootState } from "../lib/slice";
import theme from "../lib/theme";
import { Tab, TabList, Tabs, TabPanel } from "./TabWindow";

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

const ConstituentTree: React.FC = () => {
  const trees = useSelector<RootState, LabeledTree[]>((state)=>state.trees);
  const  GraphPanel = trees.map((tree) => (
      <Graph
        key={JSON.stringify(tree)}
        nodes={createNodes(tree)}
        links={createEdges(tree)}
        height="400"
        fitBoundaries={true}
        shape="circle"
      />
    ));

  const JsonPanel = <JsonTree data={trees} theme={theme} />;

  return (
    <Tabs className="window" defaulTarget="visualize">
      <div className="title-bar">
        <div className="title-bar-text">Constituent Tree</div>
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
          {GraphPanel}
        </TabPanel>
        <TabPanel role="tabpanel" title="json">
          {JsonPanel}
        </TabPanel>
      </div>
    </Tabs>
  );
};

export default ConstituentTree;
