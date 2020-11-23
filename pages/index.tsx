import Head from "next/head";
import React, { useCallback, useEffect, useState } from "react";
import SentenceForm from "../components/SentenceForm";
import ConstituentTreeViewer from "../components/ConstituentTreeViewer";
import GrammarViewer from "../components/GrammarViewer";
import CCGTreeViewer from "../components/CCGTreeViewer";
import { LabeledTree } from "../lib/labeled_tree";
import request from "../lib/create_request";
import GridLayout from "react-grid-layout";
import { useAsync } from "react-async";
interface AppState {
  grammar: LabeledTree[][];
  sentence?: string;
}

function App(): JSX.Element {
  const [state, setState] = useState<AppState>({
    grammar: [],
    sentence: undefined,
  });
  const {
    data: constituentTrees,
    isPending: isConstituentTreesPending,
  } = useAsync({
    promiseFn: useCallback(async () => {
      if (!state.sentence) {
        return [];
      }
      return await request("process", state.sentence, {
        annotators: "tokenize,ssplit,pos,lemma,ner,parse",
        "parse.binaryTrees": "true",
      }).then((response: any) => response.sentences.map((x) => x.binaryParse));
    }, [state.sentence]),
  });
  const { data: grammars, isPending: isGrammarsPending } = useAsync({
    promiseFn: useCallback(async () => {
      if (isConstituentTreesPending) {
        return [];
      }
      return await Promise.all(
        (constituentTrees as LabeledTree[]).map((constituentTree) =>
          request("extract", constituentTree)
        )
      );
    }, [constituentTrees]),
  });
  useEffect(() => {
    if (isGrammarsPending) {
      return;
    }
    setState({ ...state, grammar: grammars.map((g) => g[0]) });
  }, [grammars]);
  return (
    <>
      <Head>
        <title>CCG Playground</title>
        <link rel="icon" href="/favicon.ico" />
      </Head>
      <GridLayout className="layout" cols={2} rowHeight={400} width={1800}>
        <div key="sentence-form" data-grid={{ x: 0, y: 0, w: 1, h: 1 }}>
          <SentenceForm
            handleSentenceUpdate={(sentence: string) => {
              setState({
                ...state,
                sentence,
              });
            }}
          />
        </div>
        <div
          key="constituent-tree-viewer"
          data-grid={{ x: 0, y: 1, w: 1, h: 1 }}
        >
          <ConstituentTreeViewer constituentTrees={constituentTrees} />
        </div>
        <div key="grammar-viewer" data-grid={{ x: 1, y: 0, w: 1, h: 1 }}>
          <GrammarViewer
            grammars={grammars as LabeledTree[][][]}
            handleGrammarSelection={(grammar: LabeledTree[][]) => {
              setState({ ...state, grammar });
            }}
          />
        </div>
        <div key="ccg-tree-viewer" data-grid={{ x: 1, y: 1, w: 1, h: 1 }}>
          <CCGTreeViewer grammars={state.grammar} />
        </div>
      </GridLayout>
    </>
  );
}

export default App;
