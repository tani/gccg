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
