import Head from "next/head";
import React from "react";
import axios, { AxiosRequestConfig, AxiosResponse } from "axios";
import { v4 as uuidv4 } from "uuid";
import JsonTree from "react-json-tree";
import { Button, Spinner } from "react-bootstrap";
import {
  LabeledLabelTree,
  LabeledTree,
} from "../lib/labeled_tree";
import * as MathJax from "mathjax3-react"

const theme = {
  scheme: "twilight",
  author: "david hart (http://hart-dev.com)",
  base00: "#1e1e1e",
  base01: "#323537",
  base02: "#464b50",
  base03: "#5f5a60",
  base04: "#838184",
  base05: "#a7a7a7",
  base06: "#c3c3c3",
  base07: "#ffffff",
  base08: "#cf6a4c",
  base09: "#cda869",
  base0A: "#f9ee98",
  base0B: "#8f9d6a",
  base0C: "#afc4db",
  base0D: "#7587a6",
  base0E: "#9b859d",
  base0F: "#9b703f",
};

interface SentenceFormProps {
  handleSentenceUpdate(sentence: string): void;
}

function SentenceForm({ handleSentenceUpdate }: SentenceFormProps) {
  const [sentence, setSentence] = React.useState("");
  return (
    <form
      onSubmit={(event) => {
        handleSentenceUpdate(sentence);
        event.preventDefault();
      }}
    >
      <textarea onChange={(event) => setSentence(event.target.value)} />
      <Button type="submit">Submit</Button>
    </form>
  );
}

interface ConstituentTreeViewerProps {
  constituentTrees?: LabeledTree[];
}

function ConstituentTreeViewer(props: ConstituentTreeViewerProps): JSX.Element {
  if (!props.constituentTrees) {
    return <Spinner animation="border" />;
  }
  return <JsonTree data={props.constituentTrees} theme={theme} />;
}

interface GrammarViewerProps {
  handleGrammarSelection(grammar: LabeledTree[]): void;
  grammars?: LabeledTree[][][];
}

function GrammarViewer(props: GrammarViewerProps): JSX.Element {
  if (!props.grammars) {
    return <Spinner animation="border" />;
  }
  return <JsonTree data={props.grammars} theme={theme} />;
}

interface CCGTreeViewerProps {
  grammar?: LabeledTree[];
}

function RenderMathJaxSrcCategory(tree: LabeledLabelTree): string {
  if ("children" in tree) {
    switch (tree.label) {
      case "left":
        return `(${RenderMathJaxSrcCategory(
          tree.children[0]
        )}/${RenderMathJaxSrcCategory(tree.children[1])})`;
      case "right":
        return `(${RenderMathJaxSrcCategory(
          tree.children[0]
        )}\\${RenderMathJaxSrcCategory(tree.children[1])})`;
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
      )}\\BIC{${RenderMathJaxSrcCategory(tree.label)}}`;
    } else if (tree.children.length === 1) {
      return `${RendeMathJaxSrcDerivation(
        tree.children[0]
      )}\\BIC{${RenderMathJaxSrcCategory(tree.label)}}`;
    }
  } else {
    return tree.label;
  }
}

function renderProoftree(tree: LabeledTree): JSX.Element {
  return <MathJax.Formula formula={`$$${RendeMathJaxSrcDerivation(tree)}$$`} />
}

function CCGTreeViewer(props: CCGTreeViewerProps) {
  const [derivations, setState] = React.useState(undefined);
  React.useEffect(() => {
    if (!props.grammar) {
      setState(undefined);
      return;
    }
    axios
      .request(createRequest("construct", props.grammar))
      .then((response: AxiosResponse) => {
        setState(response.data.result);
      });
  }, [props.grammar]);
  if (!derivations) {
    return <Spinner animation="border" />;
  }
  return <JsonTree data={derivations} theme={theme} />;
}

function createRequest(
  method: string,
  ...params: unknown[]
): AxiosRequestConfig {
  return {
    method: "POST",
    url: "./api/proxy",
    data: {
      jsonrpc: "2.0",
      id: uuidv4(),
      method,
      params,
    },
  };
}

interface AppState {
  constituentTrees?: LabeledTree[];
  grammars?: LabeledTree[][][];
  grammar?: LabeledTree[];
  sentence?: string;
}

function App(): JSX.Element {
  const [state, setState] = React.useState<AppState>({
    constituentTrees: undefined,
    grammars: undefined,
    grammar: undefined,
    sentence: undefined,
  });
  React.useEffect(() => {
    if (!state.sentence) {
      setState({
        ...state,
        constituentTrees: undefined,
        grammars: undefined,
        grammar: undefined,
      });
      return;
    }
    axios
      .request(
        createRequest("process", state.sentence, {
          annotators: "tokenize,ssplit,pos,lemma,ner,parse",
          "parse.binaryTrees": "true",
        })
      )
      .then((response: AxiosResponse) => {
        setState({
          ...state,
          constituentTrees: response.data.result.sentences.map(
            (x) => x.binaryParse
          ),
        });
      });
  }, [state.sentence]);
  React.useEffect(() => {
    if (!state.constituentTrees) {
      setState({ ...state, grammars: undefined, grammar: undefined });
      return;
    }
    axios
      .all(
        state.constituentTrees.map((tree) =>
          axios.request(createRequest("extract", tree))
        )
      )
      .then((responses: AxiosResponse[]) => {
        const grammars: LabeledTree[][][] = responses.map((r) => r.data.result);
        const grammar = grammars[0][0];
        setState({ ...state, grammar, grammars });
      });
  }, [state.constituentTrees]);
  return (
    <>
      <Head>
        <title>CCG Playground</title>
        <link rel="icon" href="/favicon.ico" />
      </Head>
      <SentenceForm
        handleSentenceUpdate={(sentence: string) => {
          setState({
            ...state,
            sentence,
          });
        }}
      />
      <ConstituentTreeViewer constituentTrees={state.constituentTrees} />
      <GrammarViewer
        grammars={state.grammars}
        handleGrammarSelection={(grammar: LabeledTree[]) => {
          setState({ ...state, grammar });
        }}
      />
      <CCGTreeViewer grammar={state.grammar} />
    </>
  );
}

export default App;
