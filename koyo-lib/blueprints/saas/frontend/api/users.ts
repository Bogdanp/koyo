import axios from "axios";

export interface User {
  id: number;
  roles: string[];
  username: string;
  "created-at": string;
  "updated-at": string;
}

export const getMe = async (): Promise<User> => {
  return (await axios.get("/api/v1/users/me")).data;
};

export interface CreateUserData {
  username: string;
  password: string;
}

export const createUser = async (data: CreateUserData): Promise<void> => {
  await axios.post("/api/v1/users", data);
};

export interface CreatePasswordResetTokenData {
  username: string;
}

export const createPasswordResetToken = async (
  data: CreatePasswordResetTokenData,
): Promise<void> => {
  await axios.post("/api/v1/password-reset-tokens", data);
};

export interface ResetPasswordData {
  id: number;
  token: string;
  password: string;
}

export const resetPassword = async (data: ResetPasswordData): Promise<void> => {
  const { id, token, password } = data;
  await axios.patch(`/api/v1/users/${id}/password`, { token, password });
};
