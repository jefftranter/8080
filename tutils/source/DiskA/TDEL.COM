�8   ��;�K�÷T"T"Z"ó÷������Ä�=�]�zÌæÑ��2͵��^#V#�*~#fo��^#� *~#fo��^#V#�*n��^#� *n��^#V#��	~#fo��^#��& 	~#fo�! �+�!  �#�! �+�!  �#�! �+�! �+� ��  �� ��  �� �� ��}��|���z��{���|��}���|���z��7��|���|��7�z��Z��Z��)��|/g}/o#�  |����͉��k|/g}/o#���k��o&�9�DM��:����~#f|��/g}/o#:�<2����q��DM!  x��xGyO҃�)��v|���͔`i��|���)Ö�x  ���ڷ�z/W{/_ѯzW{_�=��yOxGæ�2��Z��Z�͉�O|���}���ȯ|g}o����)��|/g}/o#�z/W{/_�! 9~#fo�! �! �!
 �! �! �! �p! 9�~#�A���"�!U�*"�!�"�!�Y"h>�2~>�2�>�2�2��>� � ʉ� !� �F#x� ��~#�!� W�2�~#� ʳ����"��2���+}|~#����G:��x����"���2�+w#� ��+6 #ó�!�6 #� �A�2�2g>�                                                                                         *��          !  9"�* ����*���*�*��|��+�Ü�� W�2�!���>����o& �����=�¹�	�
�ڳ~�2���������� ���56 ��!  �+�W���?��_ !�~�7�z�?������:> �3���@w#�Y�q�.�D�Y�w w#w#w#w�����*>?�|���zw#�Z�����q> w#�|��������ң� �� �2����& ��0Ox������G��Ҧ�/���Ï�xØ���.7��:7��7��!��a��{�� �� ���	����0��:?�ŷO�"�!��y����$ ��7�����o& ))T])))�!�y������ �� 2����_ � ���������:��Rzw#�Z�����q> w#�|��������ң�                                                                                                               ,  User/drive designators are not permitted.
 Use "TDEL -h" for full help.
 -h	This message.
 @-a	Delete all wildcarded filespecs without confirmation prompt.
 (
Examples:
TDEL foo.bar foo.com foo.txt
 I  Must use unambiguous filenames if terminal firmware is prior to v5.2.

 -TDEL -a foo.bar *.txt foobar foo.com bar.com
 @  Expand and delete all files without confirmat�!��9�DM! 	6 `i6#6 ͐ �͐a���ҍ	!c 	~#fo�͐ )�~#fon}�-	!c 	~#fo�͐ )�~#fo#n}�A���H���?����! 	4�	! ����!c 	~#fo�͐ )�~#fo�!n��V��!  ����!a 	^#Vr+s͐ �! 	s#r͐�͐a����~	!c 	~#fo�͐)��!c 	~#fo�͐#)�~#fo��s#r! 	^#Vr+s�5	Ê	`i^#Vr+sÔ͐a+|�¢	!  ������!!	 	6 !	 	����|���	!���m���!͐a)���!��!
 	s#rz���	!���m���!!
 	~#fo�͐a)��w#w! 	�w#w`i6#6 ͐ �͐a����V!
 	~#fo�͐)��! ���!���s#rz��F
!���m���!!  �! �!
 	~#fo�͐)�~#fo��"���! �!
 	~#fo�͐)�~#fo�!c 	~#fo�͐ )�~#fo�̓���|���
!c 	~#fo�͐ )�~#fo�!���V����!!
 	~#fo�͐)�~#fo##n}���
!  ����!	 	n&  |�g}�o|��"!
 	~#fo�͐)�~#fo#n}��"!���m���!!
 	~#fo�͐)�~#fo�!	 	n��s! 	^#Vr+s`i^#Vr+s�
`i  s#r! 	s#r͐ �͐����T! 	n}��!
 	~#fo�͐ )�~#fo#n}��!�!c 	~#fo�͐ #)�~#fo�!:�! 	�͘����! 	�!K�ͻ���! 	s{�����!!
 ��"�! 	n}�A��! 	4! 	n}�N��H! 	�~#fo�!c 	~#fo�͐ #)�~#fo�!
 	~#fo�͐ )�~#fo��d�����s#r`i^#Vr+s�e͐�!O��V����!�!] 9����Unrecognized option: %s
 Can't connect to terminal
 Can't allocate filespec array
 Can't allocate array element
 %s ?
 Terminal firmware prior to v5.2 can't do wildcards
 [1mY[mes/[1mN[mo/[1mA[mll Delete %s (%s)?  YNA %d file(s) deleted.
 �!��9�DM͐n&  |�g}�o|�ʑ͐###~#fo��m�! 	�w#w͐�͐n&  |�g}�o|�ʹ!7 ü!6 ��w��͐n& �͐###~#fo��I���`is#r͐ >���> ��>���> ��7>��	> ��B�E͐n&  |�g}�o|��4!
 ��"�! 	^#Vr+s�M!���m��M�M!���m�͐n&  |�g}�o|�µ��`is#r��! 	s#r! 	�~#fo�� ���|�g}�o��s#r͐|�µ͐ |�µ͐
�!���V��͐3333�� - No file
 Protocol error
 %s - No file
 �!  9DM͐|��!���m�!��!���V��!���m�!���m�!��m�!E��m�!d��m�͐|��J!���m���!!���m�!���m�!��m�!+��m�!u��m�!���m�!���m���!��Altair 8800micro DEL utility v 2.0 %s
   Delete terminal SDCard files from the CP/M environment.
   Written by: Garry Jordan.

 Usages: TDEL [-h] ufn_list (terminal firmware prior to v5.2)
 	TDEL [-a] [-h] filespec_list
   User/drive designators are not permitted.
 Use "TDEL -h" for full help.
 -h	This message.
 -a	Delete all wildcarded filespecs without confirmation prompt.
 
Examples:
TDEL foo.bar foo.com foo.txt
   Must use unambiguous filenames if terminal firmware is prior to v5.2.

 TDEL -a foo.bar *.txt foobar foo.com bar.com
   Expand and delete all files without confirmation prompting of
   wildcarded filespecs.
 �!  9DM͐�n& � |�g}�o��s͐�!3 ��w��|��0!  ��͐�n& �|�g}�o��s͐n&  |�g}�o|��x͐n& �!}��I����|��x!  ��! ��$_$._ �!��9�DM͐�ͻ� �ң!  ä͐##�͐������s{����!  ä͐#6 ͐ 6 ͐##n}��`͐##n&  ���! 	s͐##�n&  |�g}�o��s! 	n& �͐�͐ ��a���͐###�͐ �! 	n& ���s#r! 	�~#fo�! 	nѯg��s#r�u͐###�͐ ��s#r͐###~#fo�! 	s#r͐n}�!²͐�n& @ |�g}�o��s! 	^#Vr+s!��! 	�����! 	 s! 	s! 	s`is͐n}�ʛ͐n}�?��͐#6�
͐n}�*��͐#6! 	4n&  ��!  ä`in}��;͐#n}��;͐#n}�.�N`in}��T͐#n}��T!  ä!  	n}���`in}�! 	�~#fo�! 	n�g ���͐��������s#r! 	6��! 	�~#fo�! 	n�g ���͐��������s#r! 	6Í�
! 	�! 	s#r͐n}��
͐n�͐n�}���!  ä! 	^#Vr+s��͐n}�.�=`i4n&  ��.! 	n}��4!  ä! 	6 �x`in}��M! 	4�R! 	4! 	n&  ��r! 	n&  ��x!  ä! 	^#Vr+s�͐n��s! 	^#Vr+s��͐6 ! �! 9���� ,/:;<>=[]'|" �!��9�DM͐�`is#r͐ n}���͐ �͐ n& ��0���s`i^#Vr+s��͐	��m���! 	s! 	n}��! 	n& �r͐�`is#r͐ n}��o͐ n�! 	n& ��0�� ���c! 	n& ��)�͐ n& �r`i^#Vr+s�(� 333���!��9�DM! ��)�![ ��)�! 	n& ��)�!S ��)�! ��7��P |�g}�o|��C��`is`in}�R��`in}��C`in}�R�)͐�n& ��|�g}�o��s͐n& � |�g}�o|��! ��)��&! ��)�͐n& ��)��=͐�n&  |�g}�o��s! �F!  3���!��9�DM! 	n&  |�g}�o|��t͐�ͻ���)�`i�w#w! 	~#fo�͐ �n}�ʲ! 	~#fo�͐ �n& ��)�`i^#Vr+s�z! 	n&  |�g}�o|���͐  ����!  ��)�`i^#Vr+s���& 33���!��9�DM! 	 s! 	s͐�`is#r͐ n& ��a�|�ʫ͐ #n}�/�K͐ #n& ��a�|�ʫ͐ ##n}�/«͐ #n}�/͝�`! �c! �! 	s`i�~#fo�! 	nѯg��s#r͐�͍� �җ!� �[! 	�n&  |�g}�o��s͐ n& ��B�|���͐ #n}�:��! 	�n& ##��s`i�~#fo##��s#r! 	�n&  |�g}�o��s͐ n}��&͐ n}�/�͐ n}�:��&`i^#Vr+s��͐ n}��6!� �[! 	�n�! 	n& ))))� |�g}�o��s! 	n& 3333���!��9�DM͐
  ��}͐��͐�`is#r! 	^#Vr+s�! 	^#Vr+s�n��s͐n}�ʿ!
 	^#Vr+sz�͐6 ͐ 33���!��9�DM͐|���!  �`i�w#w͐ �͐����! 	^#Vr+s�6?`i^#Vr+s��͐ 33�����& o��
���,}���
�#+|��D���z��G�<�& o��!  9DM!"�! 	��p�����!  9DM͐n}�ʖ! 	^#Vr+s�n& ��"��t���!  9DM! 	�!] �! 	��p���͐6 ���!��9�DM`i�w#w! 	^#Vr+s�n}���`i^#Vr+s��͐ 33���!��9�DM͐�`is#r! 	^#Vr+s�! 	^#Vr+s�n��s{��(�͐ 33���!  9DM! 	n& ��z �|��V! 	n& ���]! 	n& & ���!  9DM! 	n& 0 ��ͯڋ! 	n& 9 �ͯ���!��9�DM`i�w#w! 	6#6 ͐	n�! 	s{�	��! 	n}� ��!	 	^#Vr+sä! 	n}�-��! 	6�#6�!	 	^#Vr+s!	 	^#Vr+s�n�! 	s ��a�|��.͐ 
 �?�! 	nѯg���`is#r��͐�͐ ��?33333���!  9DM! 	n& �ͦ �|�ͣ�n! 	n& ��z �|�ͣ���!��9�DM!( 	^#Vr+s�~#fo�! 	s#r͐(�! 	s#r! 	^#Vr+s�n�`is{��Q `in}�%�" ! 	�! 	s#r!" 	6�#6! 	 s!  	s#6 ! 	6 ͐n}�-�! 	^#Vr+s! 	6͐n}�0�! 	60͐n& ��a�}��8! 	��k!��!  	s#r͐n}�.�_! 	^#Vr+s! 	��k!��!" 	s#r͐n}�l�u! 	^#Vr+s! 	^#Vr+s�n�`is{�dʳ�u���b���x��o�
�c�P�sʊ� ����͐~#fo|��! 	^#Vr+s�6-͐�͐~#fo����s#r!  	^#Vr+s! 	6
�! 	6�! 	6�! 	6!  	�~#fo�! 	n& �! 	^#Vr+s�~#fo�! 	��� ���ѯg����s#rÁ! 	^#Vr+s�! 	^#Vr+s�~#fo��s!  	^#Vr+sÁ͐6 ! 	�! 	s#r�ͻ��! 	s#r! 	n}���!  	^#Vr+s!  ���͐*�͐,�! 	n& �!��! 9~#fo����#|���!���T ê! 	^#Vr+sz��;͐*�͐,�! 	^#Vr+s�n& �!)�! 9~#fo����#|��8!���T ��! 	n}�ʇ!  	^#Vr+s!  �҇͐*�͐,�!  �!u�! 9~#fo����#|�!���T �E� ! 	^#Vr+s�~#fo�! 	s#r͐�ͻ��! 	s#r͐"�͐�����͐"�! 	s#r!  	�~#fo�͐�����s#rà!  �T ͐*�͐,�`in& �! �! 9~#fo����#|�� !���T �N ͐*�͐,�`in& �!? �! 9~#fo����#|��N !���T ß!  �!$ 9�����!  9DM͐^#Vr+s�͐��s���!  9DM! 	n& a ��ͯڤ ! 	n& z �ͯ���!  9DM! 	n& A ��ͯ�� ! 	n& Z �ͯ���!��9�DM͐�͐	���� !͐^#Vr+s�͐���!͐0 �!͐7 ��s! & �h!͐	�͐�͐	�͉�͐��� ����`is͐	�͐�͐	��)�͐��� ���`in& #& 3���!��9�DM`i�w#w͐~#fon& ��a�}�ʸ!͐ 
 �?�͐^#Vr+s�nѯg���`is#r�y!͐ 33��� �
|�<��!*"���*����!+�*�|/g}/o#9��!��!�#"�����z���{���7�*r�*p:tOz��"��q#�"�
��
�2"� >
_� :���R"� ��H"��� �� ��                                            