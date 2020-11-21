import type { NextApiRequest, NextApiResponse } from "next";
import axios from "axios";

const route = {
  process: "http://localhost:8081",
  extract: "http://localhost:8082",
  construct: "http://localhost:8082",
};

export default async (req: NextApiRequest, res: NextApiResponse) => {
  const response = await axios.post(route[req.body.method], req.body, {
    headers: req.headers,
  });
  res.status(200).json(response.data);
};
