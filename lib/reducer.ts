import Axios, { AxiosRequestConfig, AxiosResponse } from "axios";
import { produce } from "immer";
import { v4 } from "uuid";
import { RequestActions, ResponseActions } from "./actions";
import { JsonRpcRequest, JsonRpcResponse } from "./jsonrpc";
import { LabeledTree } from "./labeled_tree";
import { AppStore } from "./store";

export const withSideEffect = (
  dispatch: React.Dispatch<ResponseActions | RequestActions>
): React.Dispatch<RequestActions> => (action: RequestActions) => {
  switch (action.type) {
    case "GenerateTreesRequest":
      (async () => {
        const config: AxiosRequestConfig = {
          headers: {
            "Content-Type": "application/json",
          },
        };
        const data: JsonRpcRequest = {
          jsonrpc: "2.0",
          id: v4(),
          method: "process",
          params: [
            action.text,
            {
              annotators: "tokenize,ssplit,pos,lemma,ner,parse",
              "parse.binaryTrees": "true",
            },
          ],
        };
        const response = await Axios.post<JsonRpcRequest, AxiosResponse<JsonRpcResponse<any, any>>>(
          "./api/proxy",
          data,
          config
        );
        if ("error" in response.data) {
          throw Error(JSON.stringify(response.data.error));
        }
        const trees = response.data.result.sentences.map((s) => s.binaryParse) as LabeledTree[];
        dispatch({
          type: "GenerateTreesResponse",
          trees,
        });
        for (let index = 0; index < trees.length; index++) {
          withSideEffect(dispatch)({
            type: "ExtractGrammarRequest",
            index,
            tree: trees[index],
          });
        }
      })();
      dispatch(action);
      break;
    case "ExtractGrammarRequest":
      (async () => {
        const config: AxiosRequestConfig = {
          headers: { "Content-Type": "application/json" },
        };
        const data: JsonRpcRequest = {
          jsonrpc: "2.0",
          id: v4(),
          method: "extract",
          params: [action.tree],
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
        dispatch({
          type: "ExtractGrammarResponse",
          grammars,
          index: action.index,
        });
        withSideEffect(dispatch)({
          type: "ConstructDerivationRequest",
          index: action.index,
          grammar: grammars[0],
        });
      })();
      dispatch(action);
      break;
    case "ConstructDerivationRequest":
      (async () => {
        const config: AxiosRequestConfig = {
          headers: { "Content-Type": "application/json" },
        };
        const data: JsonRpcRequest = {
          jsonrpc: "2.0",
          id: v4(),
          method: "construct",
          params: [action.grammar],
        };
        const response = await Axios.post<JsonRpcRequest, AxiosResponse<JsonRpcResponse<any, any>>>(
          "./api/proxy",
          data,
          config
        );
        if ("error" in response.data) {
          throw Error(JSON.stringify(response.data.error));
        }
        const derivations = response.data.result as LabeledTree[];
        dispatch({
          type: "ConstructDerivationResponse",
          derivations,
          index: action.index,
        });
      })();
      dispatch(action);
  }
};

export const reducer: React.Reducer<AppStore, ResponseActions | RequestActions> = (prevState, action) => {
  return produce(prevState, (draftState) => {
    switch (action.type) {
      case "GenerateTreesResponse":
        draftState.trees = action.trees;
        break;
      case "ExtractGrammarResponse":
        draftState.allGrammars[action.index] = action.grammars;
        break;
      case "ConstructDerivationResponse":
        draftState.derivations[action.index] = action.derivations;
        break;
      case "ConstructDerivationRequest":
        draftState.grammars[action.index] = action.grammar;
        break;
      case "ExtractGrammarRequest":
        draftState.trees[action.index] = action.tree;
        break;
      case "GenerateTreesRequest":
        draftState.text = action.text;
        break;
    }
  });
};
