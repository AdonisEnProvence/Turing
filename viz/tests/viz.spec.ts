
// test.describe.configure({ mode: "parallel" });

// vizSpecModel.getPaths().slice(0, 1).forEach((path) => {
//   test(path.description, async ({ page }) => {
//     await page.goto("http://localhost:3000");

//     try {
//       await path.test({
//         states: {
//           "Machine execution.Page loaded": async () => {
//             await expect(page.locator('text="Turing"')).toBeVisible();
//           },
//           "Machine execution.Machine loaded": async () => {
//             await expect(page.locator('text="Running"')).toBeVisible();
//           },
//           "Machine execution.Machine execution failed": async () => {
//             await expect(
//               page.locator('text="Unknown server error"')
//             ).toBeVisible();
//           },
//           "Machine execution.Input is invalid": async () => {
//             await expect(
//               page.locator('text="An error occured during machine execution"')
//             ).toBeVisible();
  
//             await expect(
//               page.locator("text=is not in the alphabet")
//             ).toBeVisible();
//           },
//           "Editor state.Stale": async () => {
//             await expect(page.locator('text="Stale"')).toBeVisible();
//           },
//         },
//         events: {
//           "Load machine": async () => {
//             await page.locator('text="Load"').click();
//           },
//           "Make server execution fail": () => {
//             page.route("http://localhost:8080/execute-machine", (route) => {
//               route.fulfill({
//                 status: 500,
//               });
//             });
//           },
//           "Set input": async ({ event }) => {
//             if (event.type !== "Set input") {
//               throw new Error("Incorrect event");
//             }
  
//             const { input } = event;
  
//             await page.locator('css=[placeholder="0101101..."]').fill(input);
//           },
//         },
//       });
//     } catch (err) {
//       console.log(JSON.stringify(err))
//     }
//   });
// });
