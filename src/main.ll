declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
define i32 @main() {
label0:
	br label %label1
label1:
	br label %label5
label2:
	br label %label4
label3:
	;return 0 
	ret i32 0
label4:
	br label %label3
label5:
	br label %label2
}
