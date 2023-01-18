const step = 0.0001 // le pas dans le flot gradient
const coef = 0.7 // coef dans la fonction de coût d'erreur qui est une fonction exponentielle

const dercost = (x, b) => b ? -coef * Math.exp(-coef * x) : coef * Math.exp(coef * x)

export const runStepImpl = delta => ({patterns, inputs}) => st => {
    const {hiddenThresholds, hiddenWeights, finalThresholds, finalWeights, output} = st
    const finalThresholds2 = [...finalThresholds]
    const finalWeights2 = finalWeights.map (x => [...x])
    const hiddenThresholds2 = [...hiddenThresholds]
    const hiddenWeights2 = hiddenWeights.map (x => [...x])

    for (let n = 0; n < 24; n++) {
        const {selected, symbol} = patterns[n]
        if (selected) {
            const {final, hidden} = output[n]
            const input = inputs[n]

            for (let i = 0; i < 4; i++)
                finalThresholds2[i] += 10*step*dercost(final[i], i==symbol)

            for (let i = 0; i < 4; i++)
                for (let j = 0; j < 6; j++)
                    finalWeights2[i][j] -= step*dercost(final[i], symbol==i)*hidden[j].value

            for (let k = 0; k < 6; k++)
                for (let j = 0; j < 4; j++)
                    hiddenThresholds2[k] += step*dercost(final[j],symbol==j)*hidden[k].cut*hiddenWeights[j][k]

            for (let k = 0; k < 6; k++)
                for (let i = 0; i < 6; i++)
                    if (delta[k][i])
                        for (let j = 0; j < 4; j++)
                            hiddenWeights2[k][i] -=
                                step*dercost(final[j],symbol==j)*finalWeights[j][k]*input[i]*hidden[k].cut
        }
    }
    return {...st,
            finalThresholds: finalThresholds2,
            finalWeights: finalWeights2,
            hiddenThresholds: hiddenThresholds2,
            hiddenWeights: hiddenWeights2    
        }
}