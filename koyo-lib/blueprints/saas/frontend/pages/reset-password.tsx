import { Stack } from "@chakra-ui/react";
import { zodResolver } from "@hookform/resolvers/zod";
import * as React from "react";
import { useForm } from "react-hook-form";
import { useNavigate, useParams } from "react-router";
import { z } from "zod";

import { resetPassword } from "../api";
import { Button } from "../components/chakra/button";
import { Field } from "../components/chakra/field";
import { PasswordInput } from "../components/chakra/password-input";
import { useLoading, useTitle, useToaster } from "../hooks";
import { AuthLayout } from "../layouts/auth";

const formSchema = z.object({
  password: z.string().min(8),
});

type FormData = z.infer<typeof formSchema>;

export const ResetPasswordPage = () => {
  useTitle("Reset Password");

  const params = useParams();
  const {
    handleSubmit,
    formState: { errors },
    register,
  } = useForm<FormData>({
    resolver: zodResolver(formSchema),
  });
  const withToaster = useToaster();
  const [isLoading, withLoading] = useLoading();
  const navigate = useNavigate();
  const onSubmit = handleSubmit((data) => {
    withLoading(async () => {
      await withToaster(async () => {
        await resetPassword({
          id: Number(params.id),
          token: params.token || "",
          password: data.password,
        });
        navigate("/login");
      });
    });
  });

  return (
    <AuthLayout
      title="Reset Your Password"
      subtitle="Enter your new password below."
    >
      <form onSubmit={onSubmit}>
        <Stack gap="6">
          <Field
            label="Password"
            invalid={!!errors.password}
            errorText={errors.password?.message}
          >
            <PasswordInput {...register("password")} />
          </Field>
          <Button loading={isLoading} type="submit">
            Reset Password
          </Button>
        </Stack>
      </form>
    </AuthLayout>
  );
};
