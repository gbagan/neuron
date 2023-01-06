/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["dist/bundle.js"],
  theme: {
    extend: {
      colors: {
        'white': '#ffffff',
        'transp-grey': 'rgba(127, 140, 141, 0.7)'
      }
    },
  },
  plugins: [],
}
