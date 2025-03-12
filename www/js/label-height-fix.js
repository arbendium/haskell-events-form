
;(() => {
    const singleRowHeight = 19 // px

    const labels = document.querySelectorAll('label.bmd-label-floating')
    for (const l of labels) {
        const rows = Math.min(3,
            Math.max(1,
                Math.round(l.offsetHeight / singleRowHeight)
            )
        )
        l.classList.add(`label-rows-${rows}`)
    }
})()
