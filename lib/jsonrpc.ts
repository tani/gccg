import Axios, { AxiosRequestConfig, AxiosResponse } from "axios";
import { v4 } from "uuid";

export interface JsonRpcRequest<Params extends unknown[] = unknown[]> {
  jsonrpc: "2.0",
  id: string,
  method: string,
  params: Params
}

interface JsonRpcSuccess<Result = unknown> {
  jsonrpc: "2.0",
  id: string,
  result: Result
}

interface JsonRpcError<Error = unknown> {
  jsonrpc: "2.0",
  id: string,
  error: Error
}

export type JsonRpcResponse<S = unknown, E = unknown> = JsonRpcSuccess<S> | JsonRpcError<E>

export const jsonrpcRequest = async <T>(method: JsonRpcRequest["method"], ...params: JsonRpcRequest["params"]): Promise<T> => {
  const config: AxiosRequestConfig = {
    headers: { "Content-Type": "application/json" },
  };
  const data: JsonRpcRequest = {
    jsonrpc: "2.0",
    id: v4(),
    method,
    params,
  };
  const response = await Axios.post<JsonRpcRequest, AxiosResponse<JsonRpcResponse<T, unknown>>>(
    "./api/proxy",
    data,
    config
  );
  if ("error" in response.data) {
    throw Error(JSON.stringify(response.data.error));
  }
  return response.data.result;
};

