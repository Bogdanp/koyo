import * as React from "react";

export type WithLoading<T> = (proc: () => Promise<T>) => Promise<T>;

export const useLoading = <T>(initial?: boolean): [boolean, WithLoading<T>] => {
  const [isLoading, setIsLoading] = React.useState(!!initial);
  const withLoading = React.useCallback(
    async <T>(proc: () => Promise<T>): Promise<T> => {
      setIsLoading(true);
      try {
        return await proc();
      } finally {
        setIsLoading(false);
      }
    },
    [],
  );
  return [isLoading, withLoading];
};
