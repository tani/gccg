import Head from "next/head";
import React, { useEffect, useState } from "react";
import axios, { AxiosRequestConfig, AxiosResponse } from "axios";
import { v4 as uuidv4 } from "uuid";
import JsonTree from "react-json-tree";
import { Button, Spinner, Tab, Tabs } from "react-bootstrap";
import { LabeledLabelTree, LabeledTree } from "../lib/labeled_tree";
import { mathjax } from "mathjax-full/js/mathjax";
import { TeX } from "mathjax-full/js/input/tex";
import { SVG } from "mathjax-full/js/output/svg";
import { liteAdaptor } from "mathjax-full/js/adaptors/liteAdaptor";
import { RegisterHTMLHandler } from "mathjax-full/js/handlers/html";
import { AllPackages } from "mathjax-full/js/input/tex/AllPackages";
import { LiteElement } from "mathjax-full/js/adaptors/lite/Element";
import Graph from "dagre-d3-react";
import hash from "object-hash";
import NumericInput from "react-numeric-input";
const adaptor = liteAdaptor();
RegisterHTMLHandler(adaptor);
const tex = new TeX({ packages: AllPackages });
const svg = new SVG({ fontCache: "none" });
const mathDocument = mathjax.document("", { InputJax: tex, OutputJax: svg });

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
  const [sentence, setSentence] = useState("");
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

function ConstituentTreeViewer(props: ConstituentTreeViewerProps): JSX.Element {
  if (!props.constituentTrees) {
    return <Spinner animation="border" />;
  }
  return (
    <Tabs defaultActiveKey="visualize">
      <Tab eventKey="visualize" title="Visualize">
      {props.constituentTrees.map((tree) => {
        return (
          <Graph
            key={hash(tree)}
            nodes={createNodes(tree)}
            links={createEdges(tree)}
            width="500"
            height="600"
            shape="circle"
          />
        );
      })}
      </Tab>
      <Tab eventKey="JSON" title="JSON">
        <JsonTree data={props.constituentTrees} theme={theme} />
      </Tab>
    </Tabs>
  );
}

function renderGrammar(grammar: LabeledTree[]) {
  const body = grammar
    .map((rule) => {
      if ("children" in rule && rule.children.length === 1) {
        const word = rule.children[0].label;
        const category = RenderMathJaxSrcCategory(rule.label);
        return `${word} &\\rightarrow ${category}`;
      } else {
        throw new Error(`Malformed Grammar: ${rule}`);
      }
    })
    .join("\\\\");
  return `\\begin{align*}${body}\\end{align*}`;
}

interface GrammarViewerProps {
  handleGrammarSelection(grammar: LabeledTree[]): void;
  grammars?: LabeledTree[][][];
}

function GrammarViewer(props: GrammarViewerProps): JSX.Element {
  const [state, setState] = useState({
    sentence: 0,
    grammar: 0,
  });
  if (!props.grammars) {
    return <Spinner animation="border" />;
  }
  return (
    <Tabs defaultActiveKey="visualize">
      <Tab eventKey="visualize" title="visualize">
        <NumericInput
          min={0}
          max={props.grammars.length - 1}
          value={state.sentence}
          onChange={(v) => {
            setState({ grammar: 0, sentence: v });
          }}
        />
        <NumericInput
          min={0}
          max={props.grammars[state.sentence].length - 1}
          value={state.grammar}
          onChange={(v) => {
            props.handleGrammarSelection(props.grammars[state.sentence][v]);
            setState({ ...state, grammar: v });
          }}
        />
        <MathJax
          src={renderGrammar(props.grammars[state.sentence][state.grammar])}
          options={{ display: true }}
        />
      </Tab>
      <Tab eventKey="json" title="JSON">
        <JsonTree data={props.grammars} theme={theme} />
      </Tab>
    </Tabs>
  );
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
        )}\\backslash ${RenderMathJaxSrcCategory(tree.children[1])})`;
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
      )}\\BIC{$${RenderMathJaxSrcCategory(tree.label)}$}`;
    } else if (tree.children.length === 1) {
      return `${RendeMathJaxSrcDerivation(
        tree.children[0]
      )}\\UIC{$${RenderMathJaxSrcCategory(tree.label)}$}`;
    }
  } else {
    return `\\AXC{$${tree.label}$}`;
  }
}

function MathJax(props: { src: string; options: any }): JSX.Element {
  const [style, setState] = useState("");
  useEffect(() => {
    try {
      const style1 = adaptor.textContent(
        svg.styleSheet(mathDocument) as LiteElement
      );
      setState(style1);
    } catch {
      return;
    }
  });
  return (
    <>
      <div
        dangerouslySetInnerHTML={{
          __html: adaptor.outerHTML(
            mathDocument.convert(props.src, props.options)
          ),
        }}
      />
      <Head>
        <style>{style}</style>
      </Head>
    </>
  );
}

function ProofTree({ tree }: { tree: LabeledTree }): JSX.Element {
  return (
    <>
      <MathJax
        src={`\\begin{prooftree}${RendeMathJaxSrcDerivation(
          tree
        )}\\end{prooftree}`}
        options={{
          display: true,
        }}
      />
    </>
  );
}

function CCGTreeViewer(props: CCGTreeViewerProps) {
  const [derivations, setState] = useState(undefined);
  useEffect(() => {
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
  return (
    <Tabs defaultActiveKey="visualize">
      <Tab eventKey="visualize" title="Visualize">
        {derivations.map((derivation) => {
          return <ProofTree key={hash(derivation)} tree={derivation} />;
        })}
      </Tab>
      <Tab eventKey="json" title="JSON">
        <JsonTree data={derivations} theme={theme} />
      </Tab>
    </Tabs>
  );
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
  const [state, setState] = useState<AppState>({
    constituentTrees: undefined,
    grammars: undefined,
    grammar: undefined,
    sentence: undefined,
  });
  useEffect(() => {
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
  useEffect(() => {
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
