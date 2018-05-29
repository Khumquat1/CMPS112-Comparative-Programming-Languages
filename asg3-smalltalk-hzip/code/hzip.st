#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"
 Kevin Duong
 keduong@ucsc.edu
 Winter 2018
 README for details
"

nl := Character nl.
priority := SortedCollection new.

FileStream extend [
   nl := Character nl.
   catFile [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << self nextLine << nl.
      ]
   ]
]

Character extend [
   isPrint [
      ^ (Character space < self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ self asString]
                  ifFalse: [(self asInteger) > 15
                     ifTrue: [((self asInteger) = 256)
                        ifTrue: [^ 'EOF']
                        ifFalse: [ ((self asInteger) = 32)
                           ifTrue: [^ 'x20']
                        ]
                     ]                                        
                     ifFalse: [^ 'x0', 
                              ((self asInteger printStringRadix:16)
                              copyFrom: 4)]
                  ]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

Object subclass: Leaf [
   |char count foundLeaf|
   char [ ^ char ]
   count [ ^ count ]
   foundLeaf [^ foundLeaf]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
      foundLeaf := true.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]
   
   newOrder: visitor [
      visitor value: char value: foundLeaf
   ]
]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
      foundLeaf := false.
   ]

   left [^ left]
   right [^ right]
   
   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
   
   newOrder: visitor[
      left newOrder: visitor.
      right newOrder: visitor.
      visitor value: char value: foundLeaf.
   ]
]

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: currBit [
      currentByte := currentByte bitAt: bitIndex put: currBit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

|readfile readfile2 writefile args|
optC := false.
optT := false.
optU := false.
args:=0.


Smalltalk arguments: ' -c -u -t '
do: [:opt :arg |
  (arg ~= nil) ifTrue: [
      args = 0 
         ifTrue:[     
            readfile := FileStream open: arg
               mode: FileStream read.
            readfile2 := FileStream open: arg
               mode: FileStream read.        
         ]
         ifFalse:[
            writefile := FileStream open: arg
               mode: FileStream write.].
     args := args + 1.
   ].
   (opt = $t) ifTrue: [
      optT := true.
   ].
  (opt = $c) ifTrue: [
      optC := true.
   ].
  (opt = $u) ifTrue: [
      optU := true.  
   ].   
].

frequencyTable := ZeroArray new: 257.
frequencyTable at:256 put:1.
encodeTable := ZeroArray new: 257.
|decodingTree|

optU 
ifTrue:[
   |end outBytes stack|
   end := false.
   outBytes := OutBits new: writefile.  
   fileBits := BitStream new: readfile.
   stack := OrderedCollection new.
   treeDone := false.
   empty := ''.
   node := ''.   
   
   [(fileBits atEnd not)] 
   whileTrue: [
      leafChar := 0.
      nextNode := fileBits nextBit.
      treeDone 
      ifTrue:[ 
         node foundLeaf
         ifTrue:[
            ((node char) = 256) 
            ifTrue:[
               end := true.
            ] 
            ifFalse:[
            stdout nextPut: (Character value: (node char)).
            node := ''.
            ].
         ]
         ifFalse:[
            nextNode = 0 
            ifTrue:[
               node := node left.
               node foundLeaf 
               ifTrue:[
                  end not 
                  ifTrue:[  
                     ((node char)=256)
                     ifTrue:[
                        end := true.
                     ]
                     ifFalse:[
                        (writefile isNil)
                        ifTrue:[
                           stdout nextPut: 
                           (Character value: (node char)).
                        ]
                        ifFalse:[
                           8 to: 1 by: -1 do: [:index |
                           outBytes writeBit: (node char) bitAt: index.
                           ].
                        ].
                        node := empty.
                     ].
                  ].
               ]
            ] 
            ifFalse:[     
               node := node right.
               node foundLeaf 
               ifTrue:[
                  end not 
                  ifTrue:[
                     ((node char)=256) 
                     ifTrue:[
                     end := true.
                     ] 
                     ifFalse:[
                        (writefile isNil) 
                        ifTrue:[
                        stdout nextPut: (Character value: (node char)).
                        ]
                        ifFalse:[
                           8 to: 1 by: -1 do: [:index |
                           outBytes writeBit: (node char) bitAt: index.
                           ].
                        ].
                        node := empty.
                     ].
                  ].
               ]
            ].
         ].
      ]
      ifFalse:[    
         nextNode = 1 
         ifTrue:[            
            (stack size >= 2) 
            ifTrue:[
               rightCh := stack removeLast.
               leftCh := stack removeLast.
               copy := 
               (Tree new: -1 count: '' left: leftCh right: rightCh).
               stack add: copy.
            ] 
            ifFalse:[
               treeDone := true.
               empty := stack removeLast.
               node := empty.
            ].
         ].
         nextNode = 0 
         ifTrue:[
            8 to:1 by:-1 do:[:i|
               leafChar := leafChar bitAt:i put:fileBits nextBit.
            ].
            leafChar = 0 
            ifTrue:[
               lastBit := fileBits nextBit.
               lastBit = 1 
               ifTrue:[
                  leafChar := leafChar bitAt:9 put:1.
               ].
            ].
            stack add: (Leaf new: leafChar count: '').
         ].
      ].
      
   ].
].

optC | optT
ifTrue:[
   [readfile atEnd not] whileTrue: [
      frequencyTable incr: readfile next asInteger.
   ].

   frequencyTable keysAndValuesDo: [:key :count |
      (count notNil and: [count > 0]) 
      ifTrue: [
         priority add: (Leaf new: key count: count).
      ]
   ].

   [priority size >= 2] 
   whileTrue: [
      |leftCh rightCh|
      leftCh := priority removeFirst.
      rightCh := priority removeFirst.
      copy := ( Tree new: (leftCh char)
         count: (leftCh count) + (rightCh count) 
         left: leftCh right: rightCh).
      priority add: copy.
   ].
   
   decodingTree := priority removeFirst.
   decodingTree inorder: [:char :string | 
      encodeTable at: char put: string.] prefix: ''.
      
   bits := ''.
   decodingTree newOrder: [:char :foundLeaf | 
   foundLeaf  
      ifTrue:[
         bin := ''.
         char = 0
         ifTrue:[ 
            bin := '000000000'
         ]
         ifFalse:[ 
         char = 256
            ifTrue:[ 
            bin := '000000001'
            ]
               ifFalse:[
                  8 to:1 by: -1 do:[:i|
                  bin := bin,((char bitAt:i) printString).
                  ]
               ].               
         ].
      bits := (bits,'0',bin).
      ]
      ifFalse:[ 
      bits := bits,'1'
      ].      
   ].

   optC  
   ifTrue:[ 
      writefile ~= nil 
      ifTrue:[
         outBytes := OutBits new: writefile.   
         bitsize := bits size.
         1 to: bitsize by: 1 do: [:index |
            outBytes writeBit: ((bits at: index) asInteger) - 48.
         ].
         outBytes writeBit: 1.
         [readfile2 atEnd not] 
         whileTrue: [
            |ordChar|
            ordChar := readfile2 next asInteger.
            byte := (encodeTable at: ordChar). 
            1 to: (byte size) by: 1 do: [:index |
               outBytes writeBit: ((byte at: index) asInteger) - 48.
            ].
         ].
         endFile := (encodeTable at: 256).
         1 to: (endFile size) by: 1 do: [:index |
            outBytes writeBit: ((endFile at: index) asInteger) - 48.
         ].     
         outBytes flushByte.
      ]
      ifFalse:[
         stdout << bits << 1.      
         [readfile2 atEnd not] 
         whileTrue: [
            ordChar := readfile2 next asInteger.
            stdout << (encodeTable at: ordChar). 
         ].
         stdout << (encodeTable at: 256).
      ].    
   ].
   optT 
   ifTrue:[
      encodeTable keysAndValuesDo: [:char :count |
      |initPadding postPadding charFreq|
      (count notNil and: [count > 0]) 
      ifTrue: [
         initPadding := ''.
         postPadding := ''.
         charFreq := (frequencyTable at: char).
      
         ((charFreq >= 10000) & (charFreq < 100000))
            ifTrue: [postPadding := ' '].
         ((charFreq >= 1000) & (charFreq < 10000))
            ifTrue: [postPadding := '  '].
         ((charFreq >= 100) & (charFreq < 1000))
            ifTrue: [postPadding := '   '].
         ((charFreq >= 10) & (charFreq < 100))
            ifTrue: [postPadding := '    '].
         ((charFreq >= 0) & (charFreq < 10))
            ifTrue: [postPadding := '     '].
         
         ((char asInteger )> 15)
            ifTrue: [
            (((char asInteger) = 32) | ((char asInteger) = 256))
               ifFalse: [initPadding := ' ']
         ].
         
         stdout << initPadding 
             << char asCharacter visible << initPadding
             << postPadding << (charFreq) << ' ' 
             << count << nl.
      ].
      ].
   ].
].



readfile close.
writefile~=nil 
ifTrue:[
   writefile close.
].
