namespace AdventOfCode.Lib

type CircularCollection<'a> = 'a array * int * int

module CircularCollection =
    let private modE a b = ((a % b) + b) % b

    let init (source: 'a seq) =
        let asArray = Seq.toArray source
        (asArray, 0, Array.length asArray)

    let moveNext ((source, index, length): CircularCollection<'a>) =
        let nextIndex = modE (index + 1) length
        (source, nextIndex, length)

    let item ((source, index, _length): CircularCollection<'a>) = source[index]

    let itemAt at ((source, index, length): CircularCollection<'a>) =
        let idx = modE (index + at) length
        source[idx]

    let index ((_source, index, _length): CircularCollection<'a>) = index

    let length ((_source, _index, length): CircularCollection<'a>) = length
