import axios from "axios";

export interface LoginData {
  username: string;
  password: string;
}

export const login = async (data: LoginData): Promise<void> => {
  await axios.post("/api/v1/sessions", data);
};
