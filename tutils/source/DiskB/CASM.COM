�8   ��;�K�÷ NhM�`ó÷������Ä�=�]�zÌæÑ��2͵��^#V#�*~#fo��^#� *~#fo��^#V#�*n��^#� *n��^#V#��	~#fo��^#��& 	~#fo�! �+�!  �#�! �+�!  �#�! �+�! �+� ��  �� ��  �� �� ��}��|���z��{���|��}���|���z��7��|���|��7�z��Z��Z��)��|/g}/o#�  |����͉��k|/g}/o#���k��o&�9�DM��:����~#f|��/g}/o#:�<2����q��DM!  x��xGyO҃�)��v|���͔`i��|���)Ö�x  ���ڷ�z/W{/_ѯzW{_�=��yOxGæ�2��Z��Z�͉�O|���}���ȯ|g}o����)��|/g}/o#�z/W{/_�! 9~#fo�! �! �!
 �! �! �! �p! 9�~#�A���"�!U�*"�!�"�!�Y"h>�2~>�2�>�2�2��>� � ʉ� !� �F#x� ��~#�!� W�2�~#� ʳ����"��2���+}|~#����G:��x����"���2�+w#� ��+6 #ó�!�6 #� �A�2�2g>�                                                                                         *��          !  9"�* ����*���*�*��|��+�Ü�� W�2�!���>����o& �����=�¹�	�
�ڳ~�2���������� ���56 ��!  �+�W���?��_ !�~�7�z�?������:> �3���@w#�Y�q�.�D�Y�w w#w#w#w�����*>?�|���zw#�Z�����q> w#�|��������ң� �� �2����& ��0Ox������G��Ҧ�/���Ï�xØ���.7��:7��7��!��a��{�� �� ���	����0��:?�ŷO�"�!��y����$ ��7�����o& ))T])))�!�y������ �� 2����_ � ���������:��Rzw#�Z�����q> w#�|��������ң�                                                                                                              (ͫ1��0͙2�1#�&:�A��w/�* B|���:�A��^1:�A���^1��4�04�4�5-~��,#�:���&:�A�>�2�A>2�A�&�-��,H�w%��č�y���'ҟ'�T4�-� .͎ú&�T4:�A���(:�%��(�I(�(�^1�2�A�2�A:�%���?(�>2�A�!  �;(#" B����nî:�A��^1:�A��n(�* B|�!  �f(#" B���2�:�A�ʀ(>2�A��_(:�A��ʚ(�4�0ı8 6̱8é(�4�0ı86̱8�6ͱ8>$2�A>2�A��:����(;%��(����(###~;%+++��(�ͮ�4>�v�T4���b)��%�:�A��	)�û�O5�Y,��HÀ5��5�6�S8Ã3Þ4�s��H��H�1�lõI�)4Û(�s8ö9�!��9�DM!D�͂�ͅ!  "\S> 2fS> 2gS {2kS{2iS{2hS!	 	  s#r! 	s#r> 2jS!  "�N! 	^#Vr+sz���	! 	^#Vr+s! 	~#fo~#fon}�-��	! 	~#fo~#fo#n�! 	s{�F�c	�C�k	�O�s	�D��	��	>2iS��	>2hS��	! 	~#fo~#fo##n}�ʠ	! 	~#fo~#fo##�!	 	s#r��	! 	^#Vr+sz���	! 	^#Vr+s�~#fo�!	 	s#r��	�
��	>2kS��	�
��	! 	~#fo~#fo�! 	s#r�	! 	~#fo|��'
!s�͂�!��͂�!��͂�!��͂�͈`i�w#w! 	~#fo�`i~#fo�n�! 	s{��t
! 	n}�.�t
`i~#foN�! 	n��s`i^#Vr+s�-
`i~#foN6 !	 	~#fo|�ʘ
!	 	~#foÛ
!N�!lN�͋��!&�!N�͎��N�"N!+�*N�͑��"N|���
*N�!-�͔���͈�*N"N*kS}���
*N�!<�͔��!lN�͗�|��!H�!lN�͎��!M�!lN�͑��"N|��<!lN�!O�͔���͈�! �!`�*N�͚���!t�*N�͚��!��*N�͚��!��*N�͚��!��*N�͚��! "^S͝|�ʸ͠|�ʫͣ+++|�«!��ͦ�ͩ*^S#"^SÈ*fS}���!��ͦ�*gS}���! �͂�!lN�ͬ�!"n}���!,�ͬ��;*N�!6�ͯ��Ͳ*N�!N�ͯ��*N�! �͵��*N�͸�*N�͸�!lN�!U�͔���! 9����BD Software CRL-format ASM Preprocessor v1.60
 Usage:	casm [-f] [-c] [-o <name>] <filename>
 -F: flag old CMAC.LIB macros if spotted
 -C: don't strip comments from input and output
 -O <name>: Call the output file <name>.ASM
 .CSM r Can't open %s
 cfp = %04x
 .ASM w Can't create %s
 
TPALOC		EQU	%04xH
 
SYS$EXTFLAG	SET	0
 SYS$EXTADDR	SET	0
 SYS$EXTSIZE	SET	0
 		ORG	TPALOC+0205H

 Aborted by ^C
 File ends, but last function is unterminated
 Fix those errors and try again... A:$$$.SUB A:$$$.SUB 
END$CRL		EQU	$-TPALOC
 		END
 %s is ready to be assembled.
 Úù3ö9�s�H:À5�HÇ-�1��5�x:�6Ã3�!��9�DM*N�!� �!6]��v���|��*�N|��*N��y�*�N+"�N)N~#fo"N*�N �kN"N*�N)`S~#fo#"^S�|���!  ��*hS}���`i�w#w`i~#fo6]n}���`i~#fo6]n}�;¢`i~#fo|��i`i~#fo+6]n& ���|��i`i^#Vr+s�:`i~#fo|�*jS}�ʆ*^S#"^Sâ!��`i~#fo6]�͂��>2jS��`i~#fo6]n}�'��`i~#fo6]n}�"����`i^#Vr+s�`i~#fo6]n}���> 2jSͅ!��*/`�͈��|��!��*/`�͈��|���*�N+++|��'! �!��͋��*1`|��7!�͋�*�N#"�N+)`S�*^S��s#r`i�w#w!1`~#fo�`i~#fo�n& ���|��|`i^#Vr+s�S!1`~#fo�`i~#fo�6 > 2�N*1`n}�<��!%�!�N�͂��!1`~#fo##n}�:��!&�!�N�͎��*1`#�!�N�͎��!�N�͑�+�`is#r��Nn}�>��`i~#fo�N6 �N*1`n}�"�A*1`#�!�N�͂��!�N�͑�+�`is#r��Nn}�"�>`i~#fo�N6 �N*1`�!�N�͂��!'�!�N�͔��"N|�!�N�͗�|�ʀ!�N�!)�͋��Í!C�!�N�͎���N!�N�*�N �kN�͂��*�N �kN"N*�N)N�*N��s#r! "^S�|��! 33��
 INCLUDE MACLIB Only %d inclusion level[s] allowed No filename specified   r Missing include file: %s
 .LIB �`�8.�H:��,��-í:�Y-ü0�!��9�DM!  "1`"/`"-`!  "+`!6]�!�_��K��!6]�!�]��K���_�"$`*$`n}�§�/*$`n�! 	s ��N�|���! 	n}�;���/*$`"-`*$`n& ��Q�|���*$`#"$`��*$`#"$`+6 !$`��T�*$`n}��*$`n}�;��/*$`"/`*$`n& ��W�|��3*$`#"$`�*$`n}��F*$`#"$`+6 !$`��T�*$`n}��a*$`n}�;�d�/*$`�!�_����!�]�"1`!  "+`*+`( ���'*$`n�! 	s ��Z�|���! 	n}�ʶ! 	n}�;¼�/��*$`#"$`Ë*$`+n& ��Q�|���*$`#"$`�$*+`#"+`+)3`�*$`��s#r*$`n& ��Q�|��*$`#"$`��*$`n}��$*$`#"$`+6 �!4��]�333��Too many operands in this instruction for me to handle
 ëÇ-��/ö1�x:À5�1ü0�2�S8��IÞ4�)4�7$��'��:�H:�O!�3Ì2Ç.�!��9�DM*/`|��!��*/`��o��|��!��*/`��o��|��*fS}��~!��*/`��o��|��!��*/`��o��|��!��*/`��o��|��~!6]�*N��r��*-`��x�#��u��`is#r*-`�`i~#fo��{��*�R#"�R+)�N�`i~#fo��s#r*�R����{!���~�Â!��*/`��o��|��D*fS}� !���~�**`}�ʰ!�́�! 	�w#w! 	~#fo�*+`���� *�S#"�S+)lS�*4]��s#r! 	~#fo)3`~#fo�*4]��{��! 	~#fo)3`~#fo�̈́�! 	^#Vr+sø*�S ���A*&`�!O�͇��!}��~�Â!��*/`��o��|��3*fS}��n!��͇�!���~�*+`|��~!���~�*3`��x�#��u�"&`*\S#"\S+)�R�*&`��s#r*3`�*&`��{��*kS}���!
 �͊�*&`�!&�͇��*kS}���!
 �͊�>2fSdU�"4]!  "bU!  "�S> 2*`*hS}��*&`�!O�*N�͍���*&`�*N��r��*N�!g�͐��Â!z�*/`��o��|��W!��*/`��o��|��*fS}��g!���~�**`}��r͓*&`�*N��r��*N�!��͐��͖! 	�w#w! 	~#fo�*bU�����! 	~#fo �k�S##n}���*&`�! 	~#fo �k�S~#fo�!��͇���>2gS! 	^#Vr+s×> 2fSÂ*iS}�ʻ!��*/`��o��|�®!��*/`��o��|�®! �*/`��o��|�®! �*/`��o��|�®! �*/`��o��|�®! �*/`��o��|�®! �*/`��o��|�®!% �*/`��o��|�®!. �*/`��o��|�ʻ*/`�!5 �́��!f �*/`��o��|���!i �*/`��o��|���!6]�*N��r��Â!o �*/`��o��|��Â*fS}��*-`|��,*/`|��,!6]�*N��r��Â**`}��7͓*-`|��*&`�*-`�*&`�!s �!�^�͙�����!�^�*N��r��! 	�w#w! 	~#fo6]n}���! 	~#fo6]n& �͜�|�®! 	~#fo6]n}�:´����! 	~#fo6]6 ! 	^#Vr+s�p! 	~#fo6]n}�:��! 	~#fo6]6 ! 	�w#w! 	~#fo�*bU���Ң! 	~#fo �k�S~#fo�*-`��o��|�ʔ! 	~#fo �k�S##n}��{!� �́�*&`�! 	~#fo �k�S~#fo�!� �͇���Ñ! 	~#fo �k�S##6�! 	^#Vr+s��! 	~#fo �k�S�*4]��s#r! 	~#fo �k�S##6*-`�*4]��{��*-`�̈́�*bU#"bU+� ���!� �́�*/`|��!6]�*N��r��Â*/`�͟�|��6!6]�*N��r��Â*+`|��u*fS}��u! 	�w#w! 	~#fo�*+`����u! 	~#fo)3`~#fo�͢�"�`|�ʞ*&`�*�`�*&`�!� �!b^�͙�����Þ! 	~#fo)3`~#fo�ͥ�|�ʿ�gÞ! 	~#fo)3`~#fo�*&`�!� �!b^�͙����! 	�w#w! 	~#fo�*bU����C! 	~#fo �k�S~#fo�! 	~#fo)3`~#fo��o��|��5Þ! 	^#Vr+s��! 	~#fo �k�S�*4]��s#r! 	~#fo �k�S##6 ! 	~#fo)3`~#fo�*4]��{��*4]�̈́�*bU#"bU! 	~#fo)3`~#fo��x��! 	~#fo)3`~#fo�!�_����!b^�!6]�ͨ����!� �*/`��o��|��6*&`�*(`#"(`+�*&`�!� �!�^�͙�����!�^�*N��r��*+` ��3!
!�́��d*&`�*(`#"(`+�*&`�!5!�!�^�͙�����!�^�*N��r���u! 	^#Vr+s�N!6]�*N��r���! 9����EQU SET DS DB DW Too many EQU lines...increase 'EQUMAX' and recompile CASM EXTERNAL 'External's for a function must appear inside the function Externals must all be together at start of function
 Too many external functions in function "%s"
 Change the NFMAX constant and recompile CASM FUNCTION 'Function' op encountered in a function.
 Did you forget an 'endfunc' op? A name is required for the 'function' op Processing the %s function...           

; The "%s" function:
 $BEG	EQU	$-TPALOC
 ENDFUNC ENDFUNCTION 'Endfunc' op encountered while not in a function $END	EQU	$
 The label %s in function %s is undefined
 RELOC DWREL DIRECT ENDDIR EXREL EXDWREL PRELUDE POSTLUDE DEFINE Old macro '%s' leftover from "CMAC.LIB" days...
 IF ENDIF END %s$L$%s	EQU	$-%s$STRT
 Re-defined label: %s, in function %s
 Label table overflow. Increase LABMAX %s$EF$%s-%s$STRT %s$L$%s DW %s$R%03d	EQU	$-%s$STRT
 Only one relocatable value allowed per DW
 %s$R%03d	EQU	$+1-%s$STRT
 �U!Ç-�!  9DM!�#�! 	~#fo��R!��|��x!! ��!�#�! 	~#fo��R!��|�ʔ!! ��!�#�! 	~#fo��R!��|�ʰ!! ��!�#�! 	~#fo��R!��|���!! ��!�#�! 	~#fo��R!��|���!! ��!�#�! 	~#fo��R!��|��"! ��!�#�! 	~#fo��R!��|�� "! ��!�#�! 	~#fo��R!��|��<"! ��!�#�! 	~#fo��R!��|��X"! ��! 	~#fo##n}�I�I#!�#�! 	~#fo��R!��|�ʅ"! ��! $�! 	~#fo��R!��|�ʡ"! ��!$�! 	~#fo��R!��|�ʽ"! ��!$�! 	~#fo��R!��|���"! ��!$�! 	~#fo��R!��|���"! ��!$�! 	~#fo��R!��|��#! ��!$�! 	~#fo��R!��|��-#! ��!$�! 	~#fo��R!��|��I#! ��!$�! 	~#fo��R!��|��e#! ��! $�! 	~#fo��R!��|�ʁ#! ��!&$�! 	~#fo��R!��|�ʝ#! ��!+$�! 	~#fo��R!��|�ʹ#! ��!1$�! 	~#fo��R!��|���#! ��!  ��MOV INR DCR INX DCX DAD MVI DB DS CPI ORI ANI ADI SUI SBI XRI ACI ORG TITLE PAGE EJECT MACRO �L$�)4À5�x:Þ4��:��/�!��9�DM>2*`!  "(`*hS}��t$*N�!�&��:$��`i�w#w`i~#fo�*�S���ҋ%!'�!b^��=$��`i~#fo)lS~#fo��@$��! 	s#r! 	~#fo ��ͩ��$! 	~#fo��$! �! 	s#r! 	�w#w! 	~#fo�! 	~#fo+����<%! 	~#fo b^�`i~#fo)lS~#fo�! 	~#fo�n��s! 	^#Vr+s��$! 	~#fo b^6 `i~#fo)lS~#fo�! 	~#fo�n& �!b^�!#'�*N��C$����`i^#Vr+s�z$*N�!1'��:$��*hS}�ʭ%*N�!9'��:$��*&`�!M'�!�^��F$���!�^�*N��I$��*hS}���%*N�!^'��:$��*&`�!h'�!�^��F$���!�^�*N��I$��*�S|��Z&*&`�*(`#"(`+�*&`�!w'�!�^��F$�����!�^�*N��I$��*&`�*&`�!�'�!�^��F$����!�^�*N��I$��*&`�*&`�*&`�!�'�!�^��F$�����!�^�*N��I$��`i�w#w`i~#fo�*�S�����&`i~#fo)lS~#fo�*&`�!�'�!�^��F$����!�^�*N��I$��`i^#Vr+sÉ&*&`�!�'�!�^��F$���!�^�*N��I$��333333��

; List of needed functions:
 		DB	' %s','%c'+80H
 		DB	0
 
; Length of body:
 		DW	%s$END-$-2
 
; Body:
 %s$STRT	EQU	$
 %s$R%03d	EQU	$+1-%s$STRT
 		JMP	%s$STRTC-%s$STRT
 %s$EF$%s	EQU	%s$STRT
 %s$EF$%s	JMP	0
 
%s$STRTC	EQU	$
 ��'�)4Þ4�!��9�DM*hS}��(*N�!f(���'��*(`�!�(�*N���'���`i�w#w`i~#fo�*(`����U(`i~#fo�*&`�!�(�*N���'����`i^#Vr+s�(*N�!�(���'��33��
; Relocation parameters:
 		DW	%d
 		DW	%s$R%03d
 
 ö(�)4À5�x:Þ4��:��/�S8��H�!��9�DM! 	�w#w*N�!+�͞(��`i�w#w`i~#fo�*\S����0*!+�!b^�͡(��`i~#fo)�R~#fo�ͤ(��! 	s#r! 	~#fo ��ͩ�2)! 	~#fo�5)! �! 	s#r! 	�w#w! 	~#fo�! 	~#fo+���қ)! 	~#fo b^�`i~#fo)�R~#fo�! 	~#fo�n��s! 	^#Vr+s�E)! 	~#fo b^6 `i~#fo)�R~#fo�! 	~#fo�n& �!b^�!&+�*N�ͧ(����`i~#fo)�R~#fo�!4+�!�^�ͪ(���!�^�*N�ͭ(��! 	�~#fo�! 	~#fo##���s#r`i^#Vr+s��(*N�!A+�͞(��! 	�~#fo###��s#r! 	~#fo ��w*!X+�Ͱ(�!�+�Ͱ(�!���ͳ(�*N�!�+�͞(��*N�!�+�͞(��*N�!�+�͞(��*N�!�+�͞(��*N�!�+�͞(��*N�!,�͞(��*N�!,�͞(��*N�!0,�͞(��*N�!>,�͞(��*N�!G,�͞(���! 9����
		ORG	TPALOC

; Directory:
 		DB	' %s','%c'+80H
 		DW	%s$BEG
 		DB	80H
		DW	END$CRL
 CRL Directory size will exceed 512 bytes;
 Break the file up into smaller chunks, please!
 

; External information:
 		ORG	TPALOC+0200H
 		IF	SYS$EXTFLAG
 		DB	0BDH
 		DW	SYS$EXTADDR
 		ENDIF
 		IF	NOT SYS$EXTFLAG
 		DB	0, 0, 0
 		ENDIF
 		DW	SYS$EXTSIZE
 !�,"�N!�,"�N!�,"�N!�,"�N!�,"�N!�,"�N!�,"�N!�,"�N!�,"�N!�,"�N!�,"�N!�,"�N!�,"�N!�,"�N!�,"�N!�,"�N!�,"�N! "�R�A B C D E H L M SP PSW AND OR MOD NOT XOR SHL SHR �-í:�;�!  9DM! 	n& ���,�|�ͣ�W-! 	n}�$͝�W-! 	n& ���,�|�ͣ�W-! 	n}�.͝�W-! 	n}�_͝���_-í:�!  9DM! 	n& ��\-�|�ͣ-! 	n}�_͝���!  9DM! 	~#fon�! 	~#fon�}�ʬ-!  ��! 	~#fon}���-! 	^#Vr+s�n�! 	^#Vr+s�n�}���-!  ��ì-! 	~#fon}���-!  ��-! ���.�H:�!  9DM! 	~#fo~#fon& ��.�|��6.! 	~#fo^#Vr+s�.���>.�<;�!  9DM! 	~#fon}��{.! 	^#Vr+s�! 	^#Vr+s�n& ��;.���s�E.! 	~#fo6 ��Ð.�x:�J�!��9�DM! 	~#fo�͊.��`is#r! 	~#fo�͊.��! 	s#r! 	~#fo�! 	~#fo����! 	s#r! 	~#fo�! 	~#fo��! 	~#fo��! 	s#r! 	~#fo|��U/`i~#fo�! 	~#fo�! 	~#fo����#�! 	~#fo�! 	~#fo��! 	~#fo�͍.���! 	�w#w! 	~#fo�! 	s#r! 	~#fo�! 	~#fo�����/! 	~#fo�! 	~#fo��! 	~#fo�! 	~#fo�n��s! 	^#Vr+s! 	^#Vr+s�m/�!
 9������/�{0�!��9�DM! 	6 ! 	^#Vr+s�n�`is{��w0`in}�'�0! 	�n&  |�g}�o��s�80`in}�
�80! 	~#fo�! ���/��`in}�_�`0! 	n}��`0! 	~#fo�!Q ���/���t0! 	~#fo�`in& ���/����/33��Ä0��I�s8�!  9DM*kS}�ʣ0! 	~#fo��~0�ú0! 	~#fo�! 	~#fo�́0������0�S8�!  9DM*^S�*N�!1�Ϳ0���! 	~#fo�! 	~#fo�! 	~#fo�Ϳ0���>2gS��
%s: %d:  � 1ü0��Iö9õI��H�!��9�DM!
 	~#fo�! 	~#fo�! 	~#fo��1���!
 ��1�*�N�`is#r`i~#fo|څ1`i~#fo)N~#fo��1�`i^#Vr+s�Z1!�1n}�ʖ1!�1��1�!����1�33��A:$$$.SUB A:$$$.SUB ÿ1�cJ�1�!��9�DM! 	~#fo�͹1��`is#rz���1!�1�ͼ1�`i~#fo33��Out of storage allocation space
 �2�x:�1�!  9DM*4]�! 	~#fo��2�#�"4]*4]�!,]����P2!R2��2���Out of text space. Increase TXTBUFSIZE and recompile CASM Ò2õ2�!  9DM! 	~#fo�͏2�|�ʰ2! ��!  ��û2Ç-�!��9�DM`i�w#w`i~#fo�*�R����3`i~#fo)�N~#fo�! 	~#fo�͸2��|��3! �3`i^#Vr+s��2!  33���3Ç-�!��9�DM`i�w#w`i~#fo�*�S����|3`i~#fo)lS~#fo�! 	~#fo��3��|��p3`i~#fo)lS~#fo�3`i^#Vr+s�*3!  33���!  9DM! 	~#fon}�ʴ3! 	^#Vr+s�n}�.±3! ��Ê3!  ����3�s;�S8�!��9�DM!
 	~#fo�! 	~#fo�! 	~#fo�ͼ3����`is#r*kS}��4! 	~#fo�!4�Ϳ3��`i~#fo33��
 fgets got --> %s
 �54�O5�	<�1�!  9DM*kS}��T4! 	~#fo��,4��y4! 	~#fo�! 	~#fo��/4��#|��y4!{4��24���Out of disk space for output file. ê4�S8�S<�1�!  9DM*kS}���4! 	~#fo�!
 	~#fo�! 	~#fo�! 	~#fo�͡4�����*5! 	~#fo�!
 	~#fo�! 	~#fo�! 	~#fo�! 	~#fo�ͤ4�����#|��*5!,5�ͧ4���Out of disk space for output file. �U5��I�!  9DM͐n}��~5! 	^#Vr+s�n& ��R5��\5���!��9�DM͐�`is#r! 	^#Vr+s�! 	^#Vr+s�n��s{�ʸ5Ò5͐ 33���!��9�DM͐�`is#r͐n}���5! 	^#Vr+s��5! 	^#Vr+s�͐n��s! 	^#Vr+s�n}���5͐ 33���56�y<áJ�	K�[KìK�^L�aL�#>�!��9�DM!��6��! 	s#rz��X6!  �M8͐ �͐ ��s#r͐##6 #6͐6͐
n}�rʛ6�a��6�w��7�8͐�!  �͐�� 6����s#rz�¼6�A8͐##�w#w͐6�8͐�! �͐�� 6����s#rz���6��7͐~#fo��#6�|��	7�8! �!���͐~#fo��&6���#|��K7! �͐ �͐~#fo��)6��� ���[7͐~#fo��,6��A8!
 	~#fo#n}�b��7`i�w#w͐ � ����7͐ �͐ �n}���7! �!���͐~#fo��&6���͐ �~#fo�͐ ���s#r͐##�~#fo�͐ �����s#r��7`i^#Vr+s�q7�8͐�͐��/6���s#rz��8�A8�8�A8!
 	~#fo#n}�b�:8͐�n&  |�g}�o��s͐�M8͐��26�!  3333���\8��I�u?�!  9DM*W8�! 	��Y8�����8��IãL�qC�!  9DM͐>�8> ���8>�¢8> ���8>�®8> ���8>�º8> ���8> ���8> ��(9�-9! 	n& ��v8���! 	n& �! ��y8����! 	n& �! ��y8����! 	n}�
�9! �! ��y8��! 	n& �! ��y8����!����͐n&  |�g}�o|��Z9͐�! 	n& ��|8����! 	n}��n9! 	n& ��! 	n}�
ʍ9͐�! 	n& ��|8����͐�! ��|8��#|�¦9!����͐�!
 ��|8������9�s8�0D�#>�^L�!  9DM͐����9!  ��͐n&  |�g}�o|��0:͐n&  |�g}�o|��:͐�! �͹9��͐�ͼ9�#|��0:!����͐�Ϳ9�͐~#fo���9����!  9DM! 	n}� ͝�v:! 	n}�	͝�v:! 	n}�
͝���!��9�DM`i�w#w! 	^#Vr+s�n}�ʥ:`i^#Vr+sÆ:͐ 33��ö:óE��E�!  9DM! 	n& �Ͱ:�|�ͣ��:! 	n& �ͳ:�|�ͣ����:�F�u?�!  9DM! 	�*�:�! 	���:���͐6 ���!  9DM! 	n& 0 ��ͯ�:;! 	n& 9 �ͯ���B;��E�!  9DM! 	n& ��?;�|��h;! 	n& ���o;! 	n& & ���y;�(F�!��9�DM͐�! 	s#r!
 	^#Vr+s!  ���;͐��v;��`is#rz���;͐6 ͐�͐�����;!  ��;͐�<! 	^#Vr+s�͐ ��s{�
��;��;Í;͐6 ͐3333���<�s8�!��9�DM! 	^#Vr+s�n�`is{��M<͐�`in& ��<��#|��J<!���P<�<!  3���\<�uG�u?�!  9DM͐�*W<�! 	��Y<�����Â<�cJ�#>�!��9�DM͐### �͉ �! 	s#r�\�! 	s#rz���<*�! 	s#r�* �s#r�*�s#r*##�w#w͐~#fo�`is#r͐ ##~#fo�͐���ڧ=͐�* �s#r͐ ##~#fo�͐����<=�\�͐ ~#fo��s#rÜ=�\�͐ �͐�))��s#r! 	s#r͐�͐ ~#fo��s#r͐##�͐ ##~#fo�͐�����s#r͐ ##�͐��s#r͐ ####�>͐ ��\�����=͐))��|<��! 	s#rz���=!  �>͐##�͐��s#r͐####��<��\�`is#r͐ �! 	s#r͐ ~#fo�`is#r��<�! 9�����!��9�DM͐++++�`is#r*�! 	s#r͐~#fo�*���ʑ>͐ �͐����~>͐ �͐~#fo����~>Ñ>͐~#fo�! 	s#r�D>͐ �͐ ##~#fo�))�͐~#fo�����>͐ ##�~#fo�͐~#fo##~#fo���s#r͐ �͐~#fo~#fo��s#r� ?͐ �͐~#fo��s#r͐�͐##~#fo�))�͐ ����Q?͐##�~#fo�͐ ##~#fo���s#r͐�͐ ~#fo��s#r�_?͐�͐ ��s#r͐�* �s#r3333��Ä?��G�;�fH�x:�!��9�DM!( 	^#Vr+s�~#fo�! 	s#r͐(�! 	s#r! 	^#Vr+s�n�`is{��eC`in}�%�6C! 	�! 	s#r!" 	6�#6! 	 s!  	s#6 ! 	6 ͐n}�-�@! 	^#Vr+s! 	6͐n}�0�*@! 	60͐n& ��{?�}��L@! 	��~?��!  	s#r͐n}�.�s@! 	^#Vr+s! 	��~?��!" 	s#r͐n}�l@! 	^#Vr+s! 	^#Vr+s�n�`is{�d��@�u�A�b�A�x�A�o�A�c�dA�sʞB� �C�C͐~#fo|�A! 	^#Vr+s�6-͐�͐~#fo����s#r!  	^#Vr+s! 	6
�$A! 	6�$A! 	6�$A! 	6!  	�~#fo�! 	n& �! 	^#Vr+s�~#fo�! 	��x?���ѯg����s#rÕA! 	^#Vr+s�! 	^#Vr+s�~#fo��s!  	^#Vr+sÕA͐6 ! 	�! 	s#r�́?��! 	s#r! 	n}��B!  	^#Vr+s!  ��B͐*�͐,�! 	n& �!�A�! 9~#fo����#|��B!���hCþA! 	^#Vr+sz��OB͐*�͐,�! 	^#Vr+s�n& �!=B�! 9~#fo����#|��LB!���hC�B! 	n}�ʛB!  	^#Vr+s!  �қB͐*�͐,�!  �!�B�! 9~#fo����#|�B!���hC�YB�3C! 	^#Vr+s�~#fo�! 	s#r͐�́?��! 	s#r͐"�͐�����B͐"�! 	s#r!  	�~#fo�͐�����s#rôA!  �hC͐*�͐,�`in& �!$C�! 9~#fo����#|��3C!���hC�bC͐*�͐,�`in& �!SC�! 9~#fo����#|��bC!���hCó?!  �!$ 9�����wCôL�!  9DM͐n&  |�g}�o|�ʜC!����͐##^#Vr+s�|��D! �͐ �͐~#fo��tC�����|���C͐�n&  |�g}�o��s!����͐##6�#6͐ �͐ ��s#r͐ ^#Vr+s�͐��s�& ���<DôL�J�[K�!��9�DM͐���UD!  íE͐n&  |�g}�o|��tD!��íE͐##~#fo �|�D!  íE͐##~#fo� ��� ���`is#r͐ �͐ �͐~#fo��3D����͐ �����D͐�n&  |�g}�o��s!��íE͐ +� �?�`is#r͐##~#fo� ��|�ʉE!� �͐ �͐ �͐ ���6D���͐##�~#fo�͐ ���s#r͐ �~#fo�͐ �����s#r! �!���͐~#fo��9D���íE͐##6 #6͐ �͐ ��s#r!  3333���!  9DM! 	n& A ��ͯ��E! 	n& Z �ͯ���!  9DM! 	n& a ��ͯ�	F! 	n& z �ͯ���!  9DM͐^#Vr+s�͐��s���4F��HãLìK�!��9�DM͐> ��LF> ��[F>��XF> ��aF�lF�+F�pG! ��.F��pG͐##^#Vr+s�|��G͐n&  |�g}�o|���F! �͐ �͐~#fo��1F����`is#r!  ���F͐##�w#w͐�n&  |�g}�o��s!���pG͐##�͐ � �?+��s#r͐ �͐ ��s#r͐ ^#Vr+s�n�! 	s͐n&  |�g}�o|��iG! 	n}��[G��F�iG! 	n}��iG�lF! 	n& 333���{G�s8�!  9DM͐��|�¨G͐�! ��xG��#|�¨G!����͐�͐��xG��#|���G!����!  ����G��G�!��9�DM͐�͐	����H͐^#Vr+s�͐���H͐0 �H͐7 ��s! & �cH͐	�͐�͐	�͉�͐���G����`is͐	�͐�͐	��)�͐���G���`in& #& 3���lH�;�!��9�DM`i�w#w͐~#fon& ��iH�}�ʹH͐ 
 �?�͐^#Vr+s�nѯg���`is#r�zH͐ 33��� :�& o��:����� ��!  �,�:�o���H�2�g��!�~��5I#�~��(IcI
� 
� !eI"aI:dI!�w�O�(I_ 4!eI6
�5*aI~#"aI�HI� ��HI
� >
G:��x��WI�� �!���o& �25O $GETLINER0013 #RANDBEG�v&RANDEND��#RANDSTRT�g)RANDEFRAND�%RANDSTRTC��
��!\ �/:��2� �5!  ����>2g+��
��
��I� >
_� :���J� ��J��� �� ���7*t|���DM*r�*p���AJ><�5J����~#x��5J��	�	�><�QJ����~+x��QJ��|��}���
|�<�sJ*"���*��ږJ+�*�|/g}/o#9͚JҖJ�#"�����z���{���7��,ҰJ>
�2��*p���/:��2� ���>� K:��):r���J=��J=>� K:��w#�w#w:�o& �5��52g��
�,�K>2g�����)~�2#"��!! �~#fo�#� ��^#Vp+q*��~#fo���UK�s#r���5���aK�	K�7�:p�,>�wK2g���! ^#V*r:t�ʠK=KàK*p���^K���*r�! �s#r����7:p�)V>�XLz�>�XL�z�2:p�,"�!  "�*t|�*���K�5��*r�� *��!�� ѷ�!L��L��LG:��G�L2g!����K*���K*t+"t*r� "r*�#"�*�! 	N#Fp+qx���K##6>2g��K2g��#�jLõIáJ�7*p���dL�:��2\ � �5���L>2g�! �*p��gL����7�:pO*r�� ���7:p�)"xV>�bMz�>	�bM�z�2:p�,"�!  "�*t|��M*�! ^#V�*x#^#V������Mr+s*��5��*r��� �� "r*��"� *���8M2g�M#"�*t+"t*�! 	N#Fp+qx���L##6>2g�M2g��