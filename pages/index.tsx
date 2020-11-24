import produce from "immer";
import Head from "next/head";
import React, { useState } from "react";
import GridLayout from "react-grid-layout";
import CCGTreeViewer from "../components/CCGTreeViewer";
import ConstituentTreeViewer from "../components/ConstituentTreeViewer";
import GrammarViewer from "../components/GrammarViewer";
import SentenceForm from "../components/SentenceForm";
import { LabeledTree } from "../lib/labeled_tree";
interface AppState {
  text: string;
  trees: LabeledTree[];
  grammars: LabeledTree[][];
}

const App: React.FC = () => {
  const [state, setState] = useState<AppState>({
    text: "",
    trees: [],
    grammars: [],
  });

  const handleSentenceUpdate = (text: string) => {
    setState(
      produce(state, (draftState) => {
        draftState.text = text;
      })
    );
  };

  const handleTreeUpdate = (trees: LabeledTree[]) => {
    setState(
      produce(state, (draftState) => {
        draftState.trees = trees;
      })
    );
  };

  const handleGrammarSelection = (grammars: LabeledTree[][]) => {
    setState(
      produce(state, (draftState) => {
        draftState.grammars = grammars;
      })
    );
  };

  return (
    <>
      <Head>
        <title>CCG Playground</title>
        <link rel="icon" href="/favicon.ico" />
      </Head>
      <GridLayout className="layout" cols={2} rowHeight={400} width={1800}>
        <div key="sentence-form" data-grid={{ x: 0, y: 0, w: 1, h: 1 }}>
          <SentenceForm handleSentenceUpdate={handleSentenceUpdate} />
        </div>
        <div
          key="constituent-tree-viewer"
          data-grid={{ x: 0, y: 1, w: 1, h: 1 }}
        >
          <ConstituentTreeViewer
            text={state.text}
            handleTreeUpdate={handleTreeUpdate}
          />
        </div>
        <div key="grammar-viewer" data-grid={{ x: 1, y: 0, w: 1, h: 1 }}>
          <GrammarViewer
            trees={state.trees}
            handleGrammarSelection={handleGrammarSelection}
          />
        </div>
        <div key="ccg-tree-viewer" data-grid={{ x: 1, y: 1, w: 1, h: 1 }}>
          <CCGTreeViewer grammars={state.grammars} />
        </div>
      </GridLayout>
    </>
  );
}

export default App;
