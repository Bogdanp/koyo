import { toaster } from "../components/chakra/toaster";

export const useToaster = () => {
  return async <T>(proc: () => Promise<T>): Promise<T> => {
    try {
      return await proc();
    } catch (error) {
      if ([400, 401, 403, 404, 410].includes(error.response?.status)) {
        toaster.create({
          type: "error",
          title: "Error",
          description: error.response.data.error.message,
        });
      }
      throw error;
    }
  };
};
