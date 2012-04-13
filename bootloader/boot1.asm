; first stage boot loader by E.Dehling (c) 2002
;
; Compile with nasm (THE freeware assmbler) to a binary file (default setting) (should be 512 bytes afterwards)
; and write it to the boot-sector of a floppy with e.g. 'dd' or 'rawwrite'
;
;
;
; FEATURES:
; - checks for a floppy disk being present
; - loads a file called 'loader.bin' in the root of the floppy-disk (any floppy!)
;   (this file doesn't have to be the first file, or something like that)
; - executes that file
;
; TODO:
; -  we have to some very basic, human readible, ERROR-message, the current 'E#1', etc. is totaly screwed up ! we do need
;    smaller code for that, the 512 bytes are all in use at the moment :-(
; -  print some welcome message ??
; -  clear screen before printing any messages. // check where the cursor was before we were loaded.
; -  The BIOS seems to pass boot device number in dl ... we do use that, but a little more might be good:
;    when this boot-device is > 0x80, we have booted from a Hard-Disk ... We might want to catch that case
;    and halt ... a Hard-Disk can't be accessed the same way as a floppy!
; -  Make a Hard Disk supporting/detecting version of the loader. also implement FAT16/FAT32/Other Filesystems
; -  Check the read loops, to read at most 3/4 times, then print error and halt ... Not just lock up!
; -  use the data from the 'disk-descriptor-table' whereever possible! otherwise its impossible to
;    easily convert the loader for other types of disks ... we might uses symbolic constants for that,
;    put them in the 'disk-descriptor', and elsewhere, they don't use more memory, and can allow pre-calculation
;    at compile-time in some cases.
; -  make code readible/adjusable for others than me! // remove disabled pieces of code that were used for debugging. Probably i
;    should make some flow diagram as GIF, and include it.
; -  put stuff in some subroutine, to make code smaller ...
; -  during the loading of the file, we might want to check for errors in the FAT! otherwise we might 'lock up'
;    or even worse, unpredictable behavior could occur.
; -  Create possibility of loading a file bigger than 64K.
; -  Make it possible to load the file to another location in the RAM, make it easy to configure where to load to. (with some CONSTANT?)
; -  CODE CLEANUP ! CODE CLEANUP ! CODE CLEANUP ! CODE CLEANUP ! CODE CLEANUP ! CODE CLEANUP ! CODE CLEANUP ! CODE CLEANUP !
;
; DISCLAIMER:
; I am not responsible for this file, and anything that has to do with this file. Whatever happens as a cause of that you downloaded
; this file is on your own risk. The only thing i have to do with the file is the creation. nothing more.
;
; LICENSE:
; You are free to do whatever you want to do with this file.
; I would like it if you send feedback by email, so just mail me if you use it. I just want to know you are interested in it :-)
; e.e.dehling@student.utwente.nl
;
; QUESTIONS AND COMMENTS:
; mail them, will you?
;
; Greetz to all the freax out there,
;   Eike.


; 16 bit program (real-mode!)
[BITS 16]


;----------------- S T A R T   O F   M A I N   P R O G R A M ------------------
;------------------------------------------------------------------------------

; We have to jump to the real start. The Disk-Description ot the Fat-Boot-Sector has to come first
   jmp real_start

; Here comes the Disk description table ...
; ATTENTION: watch out when you edit these values, they are hard-coded at several places in the code ...
;            you have to edit them there as well, to avoid buggy code...

OEMlabel:          db "EOS Boot" ; OEM label
BytesPerSector:    dw 512        ; bytes per sector
SectorsPerCluster: db 1          ; sectors per cluster
ReservedForBoot:   dw 1          ; number of reserved sectors for the boot-record
NumberOfFats:      db 2          ; number of copies of the FAT
RootDirEntries:    dw 224        ; number of entries in Root-Dir
; ==> that gives us ('RootDirEntries' * 'BytesPerDirEntry') = 224 * 32 = 7168 = 14 sectors of Root-Dir! to read in
LogicalSectors:    dw 2880       ; number of logical sectors
MediumByte:        db 0xF0       ; medium descriptor byte
SectorsPerFat:     dw 9          ; sectors per FAT
SectorsPerTrack:   dw 18         ; sectors per track/cylinder
Sides:             dw 2          ; number of sides/heads
HiddenSectors:     dw 0          ; number of hidden sectors
TotalSectors:      dd 2280       ; Total number of sectors on volume (some docs do not mention this one ... real weird thingy)
                                 ; bytes 0x24 till 0x3D are reserved for 'future use' by DOS
                                 ; whatever that may be ... I guess DOS won't use them any more :-))
Reserved4DOS:      db "                        "




real_start:
; init stack, at 8192 bytes past program start (disk-buffer is direcly behind program). We'll take 4K, which should be
; more than enough for a boot-loader :-)
   mov ax, 0x07C0
   add ax, 512
   mov ss, ax
   mov sp, 4096

; Tricky: 'lea si, [***]' won't work when we don't set ds to '0x07c0' since i first tested the loader as .com file
; under DOS, i first didn't have any problems like that ... in a .com, ds is automatically set to the correct value.
; otherwise after lea stuff is not loaded from correct location, and you get weird data ;-)
   mov ax, 0x07C0
   mov ds, ax

; Bios passes boot device number in dl, lets save it ...
   mov byte [BOOTD], dl

;
; clear eax, my BIOS docs state that older bioses get into trouble when you don't clear eax ...
   xor eax, eax

; get 'BIOS equipment list word'
; BIT#1 indicates wether (a) floppy-drive(s) is(are) present (BIT#1 set -> floppy present)
; NOTE: Finally i got that new version of Ralf Browns list ... it now includes the note,
; that AWARD BIOS v4.50G and v4.51PG erroneously return 'BIT 0 CLEARED' when a floppy-drive is
; present ... Can't Award keep up to standards ?? Those Damn idiots!
; SO: this loader might not work on those BIOSSES .... cause it thinks there is no Floppy ...
; unfortunately, one of my BIOSSES is such a bios ...
; i cannot completely confirm the bit not set statement, it is actually set about half the time ... totally f*cked up.
   int 0x11
   and eax, 1

; (or eax, eax) => fast version of (cmp eax, 0)
   or eax, eax
   jne have_floppy

; BIT #1 of BIOS-equipment-word was clear, so no floppy is present.
; since we can't boot from a hard-disk yet, we'll halt.
   lea si, [msg0]  ;-> "ER#1"
   mov ax, 0xB800
   mov es, ax
   mov di, (80 * 1 + 0) * 2
   xor ecx, ecx
   mov cx, 4
   cld
   rep movsb
   jmp $

have_floppy:

; Now that we have a floppy, lets reset the floppy, and read the first block of data ...

; reset floppy drive #[BOOTD]
   call Reset

; BIOS docs: (CF set -> error, CF clear -> succes)
   jnc floppy_ok

; CF was set, so an error occured while resetting the floppy.
   lea si, [msg1]  ;-> "ER#2"
   mov ax, 0xB800
   mov es, ax
   mov di, (80 * 1 + 0) * 2
   xor ecx, ecx
   mov cx, 4
   cld
   rep movsb
   jmp $

floppy_ok:

; Great, we are ready to read our first block of data ...
; Which is: first 512 bytes of the Root-Dir. For the moment we will only search the
; first 16 entries of the Root-Dir for the filewe want to load. This is a (kinda great)
; restriction, but we want to keep this loader within 512 bytes ...
; the fist sector of the root dir is: sector # 19
; with our disk-descriptor, that would be: sector #2, side #1, track #0

; root dir starts at Logical Sector: 19 (ReservedForBoot + NumberOfFats * SectorsRerFat)
   mov ax, 19
   call L2HTS

; es:bx should point to our buffer; i have put it at the end of our file (actually, just the files starting point)
   lea si, [Buffer]
   mov bx, ds
   mov es, bx
   mov bx, si

; int 13 params: 'read floppy sectors', 'read 14 sectors' (see 'disk descriptor block' ...)
; NOTE: on older biosses we might have to convert this to two 'reads'; reads across track-borders aren't allowed there...
   mov ah, 0x02
   mov al, 0x0E

; prepare to enter loop; CLC(clear carry-flag) might not be needed, but booting sometimes fails when i don't.
; also pusha isn't really needed (we don't have to push all regs ...) but since 'pusha' is smaller when compiled,
; than 3 single 'push'-es, i use 'pusha' (size matters more than speed in a boot-loader ...)
   clc
   pusha

read_first_sector:
   popa
   pusha
   int 0x13

; great, we should have read the first sector now ...
; lets check if an error occured ... (CF clear -> no error, CF set -> error ...)
; on error: infinite retry (my BIOS docs state to always read 3 times, because the motor cant spin up fast enough, i
; personally think nothing less than infinite should be sufficient :-)
; between the 'reads' we have to reset the drive ...

; reset floppy drive #[BOOTD]
   call Reset

   jc read_first_sector

; clean up the stack ...
   popa

; reading first sector was successful !! we rock...
; so: root-dir now starts at byte ds:[Buffer] in the memory. Lets check the Root-Dir for a file called [file2load]
; general idea:
;
; repeat 224 times
; begin
;    compare [file2load] with [Root-Dir-Entry]
;    match: go load file...
;    NOT-match: check next entry
; end
; if (file not found) then print message and halt.
;
; format of [file2load]: 8 bytes filename, 3 bytes extension. you should always use the full 8/3 bytes of name,
; to prevent errors while searching, because of mal-formed names...
;

; es:di -> [Root-Dir]
   mov ax, ds
   mov es, ax
   lea di, [Buffer]
; search all entries in root-dir ...
   mov cx, word [RootDirEntries]
; we are searching at offset 0 in root-dir!
   mov ax, 0


next_root_entry:

; swap cx and dx, cause we also use cx in the inner loop...
   xchg cx, dx

; ds:si -> [file2load]
   lea si, [file2load]
   mov cx, 11
   rep cmpsb
   je found_file_2_load

; increase number of searched entries by 1 ... (offset += 32 (bytes))
   add ax, 32

; increase root-dir-pointer to point to next entry.
   lea di, [Buffer]
   add di, ax

; swap cx and dx, cause we now need the 'outer' cx
   xchg dx, cx
   loop next_root_entry

; [file2load] not found ... (dump root-dir on screen,) print error and halt.
   lea si, [msg2]  ;-> "ER#3"
   mov ax, 0xB800
   mov es, ax
   mov di, (80 * 2 + 0) * 2
   xor ecx, ecx
   mov cx, 4
   cld
   rep movsb
   jmp $

found_file_2_load:
;------------------------------------------------------------------------------------------------------------------------
; Hex-Dump first (2 KB of Root-Dir) bytes after position of found-file, for debugging...
;  ;lea si, [Buffer]
;   xchg si, di
;   mov cx, 40
;   call print ; corrupts es, di! can't continue!
;   jmp $      ; so: we'll halt.
;------------------------------------------------------------------------------------------------------------------------

; lets load the file ...
; - fetch cluster from Root-Dir
; - Load the FAT into mem.
; - load the whole file into the memory ... (up to 64kb, of course)
; - execute the file...
; - just in case the file transfers control back: halt ...

; first we fetch the first cluster from the Root-Dir and store it in [CLUSTER]
; es:di now points to the char PAST the filename, which leaves (0x1A - 0x0B) = 0x0F
   mov ax, word [es:di+0x0F]
   mov word [CLUSTER], ax

; now we'll load the whole FAT into 'Buffer'

; sector 1 => first sector of the first FAT.
   mov ax, 1
   call L2HTS
; es:bx should point to our buffer; i have put it at the end of our file (actually, just the buffers startin point...)
   lea di, [Buffer]
   mov bx, di

; int 13 params: 'read floppy sectors', 'read 9 sectors' (see 'disk descriptor block' ...)
; NOTE: on older biosses we might have to convert this to two 'reads'; reads across track-borders aren't allowed there...
   mov ah, 0x02
   mov al, 0x09
; clear carry - cause read-sector might forget to clear - to prevent eternal-loop
   clc
   pusha

Read_Fat:
; restore registers, just in case they get screwed up during INT 0x13...
   popa
   pusha

   INT 0x13

; we should have read the Fat by now, if CF is set, an error occured ...
   jnc Read_Fat_OK

; reset floppy drive #[BOOTD]
   call Reset

   jmp Read_Fat

Read_Fat_OK:
   popa

; When we reach this point, we successfully read the FAT!
; now lets load the file ...
; here we assume: 'CLUSTER' is the starting cluster of our file.
; in the loop we'll set it to the next cluster each time ...
; if the file is bigger than 64 KB, you'll get some pretty impredictible behavior when we
; attempt to exec it: the Buffer-Pointer will overflow, and i won't guarantee anything about
; what happens when you exec a random position in the file :-)

; we load the file to 0x2000:0x0000
   mov ax, 0x2000
   mov es, ax
   xor bx, bx
; int 13 params: 'read floppy sectors', 'read 1 sectors'
   mov ah, 0x02
   mov al, 0x01
; save for the case, we/INTs screw them ...
   push ax

Load_File_Sector:

; first convert the sector to the logical sector
   call C2L

; now change that to the appropriate params for INT 0x13
   call L2HTS

; Set buffer past what we have already read.
   mov ax, 0x2000
   mov es, ax
   mov bx, word [POINTER]

; save/restore for the case we/INTs screw them
   pop ax
   push ax

   INT 0x13

; succes ?
   jnc Calculate_Next_Cluster

; an error occured while reading, so lets reset the floppy, and try again!
   call Reset
   jmp Load_File_Sector

; We successfully read the cluster, check where the next one is ...
Calculate_Next_Cluster:
;------------------------------------------------------------------------------------------------------------------------
;   mov ax, 0xB800
;   mov es, ax
;   mov di, word [COUNT]
;   inc word [COUNT]
;   inc word [COUNT]
;   mov word [es:di], '* '
;------------------------------------------------------------------------------------------------------------------------

   mov ax, [CLUSTER]
   xor dx, dx
   mov bx, 2
   div bx
; dx = [CLUSTER] mod 2; so if dx = 0 [CLUSTER] is even, if dx = 1 [CLUSTER] is odd.
   or dx, dx
; if [CLUSTER] is even, we have to drop the last 4 bits of the word with the next cluster.
; when [CLUSTER] is odd, we'lll have to drop the first 4 bits.
   jz Even

Odd:
   mov ax, [CLUSTER]
   mov bx, 3
   mul bx
   mov bx, 2
   div bx
; now we have in ax the adress of the word in the FAT in which the 12 bits are, so we'll load the whole word
   lea si, [Buffer]
   add si, ax
   mov ax, word [ds:si]
; and shift out the first 4 bits. (which belong to another entry.)
   shr ax, 4
; so: ax contains the cluster now! Store it.
   mov word [CLUSTER], ax

   cmp ax, 0x0FF8
   jae End

; Done; increase buffer-ptr; next sector
   add word [POINTER], 512
   jmp Load_File_Sector

Even:
   mov ax, [CLUSTER]
   mov bx, 3
   mul bx
   mov bx, 2
   div bx
; now we have in ax the adress of the word in the FAT in which the 12 bits are, so we'll load the whole word
   lea si, [Buffer]
   add si, ax
   mov ax, word [ds:si]
; and mask out the last 4 bits. (which belong to another entry.)
   and ax, 0x0FFF
; so: ax contains the cluster now! Store it.
   mov word [CLUSTER], ax

   cmp ax, 0x0FF8
   jae End

; increase buffer-ptr; load next sector
   add word [POINTER], 512
   jmp Load_File_Sector

End:
; clear stack.
   pop ax
; provide second boot-loader with the boot-device.
   mov dl, byte [BOOTD]
; We have loaded the whole file, so lets execute it.
   jmp 0x2000:0x0000



;----------------------------- P R O C E D U R E S ----------------------------
;-----------------------------     below here      ----------------------------



;*** Reset Floppy #'BOOTD' ***
;
; input: [BOOTD] is set to the Boot-device.
; output: CF set on error, else CF is cleared
Reset:
   push ax
   push dx
   xor ax, ax
   mov dl, byte [BOOTD]
   int 0x13
   pop dx
   pop ax
   ret
;*** end reset procedure ***



;*** Converts Cluster to logical sector. ***
;
; input: cluster is in [CLUSTER]
; output: logical sector in ax
C2L:
; first load cluster into ax.
   mov ax, word [CLUSTER]

; Add 31 (-2 because clusters start at 2, and +1 because of boot-sector...,
; and +14 for Root-Dir (see 'Disk descriptor'), and + 'NumberOfFATs' * 'SectorsPerFat' = +18 sectors for the FAT)
   add ax, 31

   ret
;*** end convert cluster procedure ***



;*** Calculate HEAD(=SIDE), TRACK(=CYLINDER), SECTOR settings for INT 0x13 ***
; input: logical sector in AX.
; output: head, track and sector; in the correct registers for INT 0x13, [BOOTD] is also put in the correct register.
;         all registers that are not used for output (all except cx, dx) are preserved!
;
; Thanx to J. Andrew McLaughlin for his free (GPL) VisOpSys-loader
; from which i ripped part of this proc!
L2HTS:

; We modify some of the registers, so save them...
   push bx
   push ax

; save logical sector, cause physical sector calculation screws AX up ...
   mov BX, AX

; First the sector: 'Physical Sector' = ('logical sector' mod 'number of sectors per track/cylinder') + 1
   xor DX, DX
   div word [SectorsPerTrack]
; Physical Sectors start at 1, not at 0 (like the logical ones do ...)
   add DL, 01h
; The remainder of a division is in dx, rememember ??
   mov cl, DL ; sectors belong into cl for int 0x13
   mov AX, BX

; Now the head: 'head' = ('logical sector' / 'number of sectors per track') mod 'number of heads/sides'
; and track = ('logical sector' / 'number of sectors per track') / 'number of heads/sides'
   xor dx, dx
   div word [SectorsPerTrack]
   xor dx, dx
   div word [Sides]
   mov dh, DL ; head/side
   mov ch, AL ; track

; restore original values in registers...
   pop ax
   pop bx

; set correct device
   mov dl, byte [BOOTD]

   ret
;*** end of HeadTrackSector proc ***



;--------------------------- S T A T I C   D A T A ----------------------------
;---------------------------  (for messages, etc)  ----------------------------

; our great, and very 'information-rich' error-messages
; i had to reduce them to almost nothing because i needed memory
msg0:      db "E 1 "  ; 4 bytes
msg1:      db "E 2 "  ; 4 bytes
msg2:      db "E 3 "  ; 4 bytes

; format of [file2load]: 8 bytes filename, 3 bytes extension. you should always use the full 8/3 bytes of name,
; to prevent errors while searching, because of mal-formed names...
file2load: db "LOADER  BIN" ; the second-stage boot loader. DOS name: 'loader2s.bin'



;----------------------------- V A R I A B L E S ------------------------------
;------------------------------------------------------------------------------

BOOTD:     db 0 ; Boot device number
CLUSTER:   dw 0 ; The cluster of the file we want to load
POINTER:   dw 0 ; a pointer into Buffer, for loading the 'file2load'.



;------------------------ T R A I L I N G   S T U F F -------------------------
;------------------------------------------------------------------------------

; fill up till 512 bytes.
db "   "

; Used by the BIOS to check wether a floppy is supposed to be booted...
dw 0xAA55

; Be sure that we locate the (disk) buffer AT THE END of our program ...
Buffer:

; 8 K from here, the stack starts. If we need a bigger buffer, we have to move stack!
