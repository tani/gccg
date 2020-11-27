import Graph from "dagre-d3-react";
import React from "react";
import { useSelector } from "react-redux";
import { LabeledTree } from "../lib/labeled_tree";
import { RootState } from "../lib/slice";
import { TabWindow } from "./TabWindow";

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
  const trees = useSelector<RootState, LabeledTree[]>((state) => state.trees);
  return (
    <TabWindow title="Constituent Tree" data={trees}>
      {trees.map((tree) => (
        <Graph
          key={JSON.stringify(tree)}
          nodes={createNodes(tree)}
          links={createEdges(tree)}
          height="400"
          fitBoundaries={true}
          shape="circle"
        />
      ))}
    </TabWindow>
  );
};

export default ConstituentTree;
