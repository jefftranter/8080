�/Usage: READ <file name> [L]
Binary copy <file name> from host environment. L = use lower case for name.

Examples
READ bdos.mac        copy BDOS.MAC to BDOS.MAC
READ bdos.mac L      copy bdos.mac to BDOS.MAC
READ *.mac           copy all files ending with .MAC
READ a:bdos.mac      copy BDOS.MAC to A:
READ a:*.com         copy all .COM files to A:
READ src/bdos.mac    copy SRC/BDOS.MAC to BDOS.MAC
READ src/a:bdos.mac  copy SRC/BDOS.MAC to BDOS.MAC on A:
READ src/*.mac       copy all .MAC files from directory SRC
READ src/*.MAC L     copy all .mac files from directory src
READ src/a:test.*    copy all TEST.* files from directory SRC to A:
$                                            

For SIMH. Copyright 2010 by Peter Schorn, peter.schorn@acm.org

                                                         kB written.
$
READ V-2.24 (12-Jun-10) SIMH Interface VXXX
$Need SIMH Interface V003 or above for wild card support.
$Macintosh OS 9 has no wild card support.
$Previous file of same name deleted.
$No more directory space available.
$File does not exist.
$File is empty.
$Could not create directory entry.
$Disk full.
$Read from "$" and write to "$".
$No file matches "$SIMH required.
$$    .125$.25$ .375$.5$  .625$.75$ .875$      �!	 ��n�f��u��! ��ng��u��
 �n�f͵�H!0 �^�V���l
 �n�f͊��%
 �n�f͏0 ���og"�*�#"�*�g����!�6! �!���ug}��o�� ͧګ!����! �!���ug}��o�� ͬ��!����^! "��!�6! �!���u! �!���ug}��o�̈́�!����^! "��!� ��l�*� g}2��*�g� ng�[Wͬ�F*�g� ng: ͬ�V!����^͕*�g� ng�[Wͬ�r!�4ê *�gͬڂê *�gͬڙ: *� gͬڤ!�6ê!�5�*�g#� ng: ͬ�l*�g� ngA ����*�g� ngP ����*�g� ng��}2���3*�g� nga ���*�g� ngp ���3*�g� ng��}2��*�g##}2��*�g++� �*�gng��s!�4*� g�[�W͵8�++}2� �*� g#� w��	 *�gͧҘ*�g+� ng. ͬ���nga ��ڮ{ ͵ڼ���ng�u*�g��^Ws!�4�! �! ��eog"�"�2�2��$ �.�*�g}2��!�6 *�g͵�v	*� g�[�W͵�0	*�g�6 !�4�t	*�g� ~2��. *�gͬ�h		 *�g͵�a	!  ��w�e	!�4�t	*�g��w!�4�!����!�6*�g� ng_Wʹڨ	*�g� ng��!�4�!����_W*�gʹ��	@ *�g��!: ��!�6 *�g͵�$
�ng  ͧ��	*�g�ng�� *�gͬ�
  *�gͧ�
!. ��!�4�!����!� �! ��e! ��! ��lg}�o���ژ
*���ڕ
 *�ͬ�o
 *�gͬ�
!J����^Ì*���ڕ
!3����^Ì��
*���! ��l��s*�#"�� ͬ��
og"��o *�ͬ��
Ì *�ͬ��
�^�=
 *����+� *�͵�
�6*�#"���o *�ͬ�Ì *�ͬ�+�^ *�͊��%�!���R� *�͏�������!D���*�g̈́�! �!���ug}��o�� ͬڌ![����^!� �! ��e *�ͬҩ ͬگ͕�!� �! ��e�}2���_W*� gʹ��bk� ng  ͬ��!� 5��!�6*� g�[�W���bk� �*�g#ng��s!�4�!� 5��w!�6�_W*�gʹ�>!� �! ��e!�5�!� �! ��e���eo�"����eo�"�!�� ��ڕ���eo�"|�}2~����8�â!�����^�b!R���!� �! ��e!� ��l}2��*�g̈́�}2�!"�*�ng$ ͧ�*�ng/ ͬ���[*�s*�#"���� *� g���  *� gͬ�$���R *� ͬ�6R ͬ�?���� *� g���l� ngL ͬ�l*� g+� ng  ͬ��*� g� 6 ��!�6*� g�[�W����bk� ngA ��ڱ*�g� ng[ ͵��*�g� �*�gng  ��s!�4�*� g����!����^�b! �og��u0 ���: *� gͬ�>*� g}2��_W*�gʹ�5bk#� �*�gng��s!�5�!� 6 !� 42�2�!�6*� g�[�W��ڊbk� ng* ͬ�y*�g� ng? ͬڄ!�6Ê!�4�_W*�gʹ�_: *gͬڰ!�����b��3 *~g͵��!�����b�!� �! ��eͰ_W*�gʹ�(!� 6 !�6*�g� �[�Ws!�4Ͱ*�g��8�*�g+}2� ���Ͱ*�g��8��\!����!�6*� g�[�W���Ubk� ng��!�4�!�����b��Õ���M�Y���M�xo�g����M��� ��o`��}�7ȯ�͏��BK�og>�j�B0	?��j�= ��ͬ?ɯ��R��7����?��|������R�����B� ?ɯ|�ȯ7�DMog>)��j�0	= ����� 	x�(#�7�����?�_W��������9�������Dg9�hg���9�����s#y��r#y� ��$$$     SUB                     ���o���	�� =� !��w=�4	>���=�g�* +���:�� >��� SPLRTLB V-1.02.01 (03-Dec-06 17:58:45) Z80 processor required.
$���	�� =� !��w=�