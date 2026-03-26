import type { Metadata } from "next";
import "./globals.css";
import { Inter } from "next/font/google";
import { Navbar } from "@/components/navbar";

//define the inter font as subset of the latin font
const inter = Inter({ subsets: ["latin"] });

export const metadata: Metadata = {
  title: "Stacks Amm",
  description: "Trade any token on stacks",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en">
      <body className={inter.className}>
        <div className="flex min-h-screen flex-col gap-8 w-full">
          <Navbar />
          {children}
        </div>
      </body>
    </html>
  );
}
