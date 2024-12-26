import { Input, Stack } from "@chakra-ui/react";
import { zodResolver } from "@hookform/resolvers/zod";
import * as React from "react";
import { useForm } from "react-hook-form";
import { z } from "zod";

import { createPasswordResetToken } from "../api";
import { Button } from "../components/chakra/button";
import { Field } from "../components/chakra/field";
import { toaster } from "../components/chakra/toaster";
import { useLoading, useTitle, useToaster } from "../hooks";
import { AuthLayout } from "../layouts/auth";

const formSchema = z.object({
  username: z.string().min(6),
});

type FormData = z.infer<typeof formSchema>;

export const ResetPasswordRequestPage = () => {
  useTitle("Reset Password");

  const {
    handleSubmit,
    formState: { errors },
    register,
  } = useForm<FormData>({
    resolver: zodResolver(formSchema),
  });
  const withToaster = useToaster();
  const [isLoading, withLoading] = useLoading();
  const onSubmit = handleSubmit((data) => {
    withLoading(async () => {
      await withToaster(async () => {
        await createPasswordResetToken(data);
        toaster.create({
          type: "success",
          title: "Check Your Email",
          description: "Follow the instructions to reset your password.",
        });
      });
    });
  });

  return (
    <AuthLayout
      title="Reset Your Password"
      subtitle="We'll send you an e-mail with instructions describing how you can recover your password."
    >
      <form onSubmit={onSubmit}>
        <Stack gap="6">
          <Field
            label="Email"
            invalid={!!errors.username}
            errorText={errors.username?.message}
          >
            <Input type="email" {...register("username")} />
          </Field>
          <Button loading={isLoading} type="submit">
            Reset Password
          </Button>
        </Stack>
      </form>
    </AuthLayout>
  );
};
