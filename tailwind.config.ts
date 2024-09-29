import type { Config } from 'tailwindcss';

export default {
  content: ["./src/**/*.{ts,tsx}"],
  theme: {
    extend: {
      height: {
        '30': '7.5rem',
      }
    }
  },
  plugins: [],
} satisfies Config