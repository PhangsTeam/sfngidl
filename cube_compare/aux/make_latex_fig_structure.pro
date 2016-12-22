FUNCTION make_latex_fig_structure,position=position,double_column=double_column,centering=centering, $
                           dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                           label=label,caption=caption,newpage=newpage,ps_file_names=ps_file_names,angle=angle

;=== Those are defalut values
use_position='H'
use_double_column=0
use_centering=1
use_dimension_type='height'
use_dimension_value=5.
use_dimension_unit='cm'
use_label='fig:somefigure'
use_caption=' This is the caption'
use_newpage=0
use_ps_file_names=ptr_new(['file1.ps','file2.ps'])
use_angle=0

;=== Check inputs
IF keyword_set(position) THEN use_position=position
IF keyword_set(double_column) THEN use_double_column=double_column
IF keyword_set(centering) THEN use_centering=centering
IF keyword_set(dimension_type) THEN use_dimension_type=dimension_type
IF keyword_set(dimension_value) THEN use_dimension_value=dimension_value
IF keyword_set(dimension_unit) THEN use_dimension_unit=dimension_unit
IF keyword_set(label) THEN use_label=label
IF keyword_set(caption) THEN use_caption=caption
IF keyword_set(newpage) THEN use_newpage=newpage
IF keyword_set(angle) THEN use_angle=angle

fig_st={position:use_position, $
        double_column:use_double_column, $
        centering:use_centering,  $
        dimension_type:use_dimension_type, $
        dimension_value:use_dimension_value, $
        dimension_unit:use_dimension_unit, $
        label:use_label, $
        caption:use_caption, $
        newpage:use_newpage, $
        ps_file_names:ptr_new(), $
        angle:use_angle}

fig_st.ps_file_names=ptr_new(['file1.ps','file2.ps'])

IF keyword_set(ps_file_names) THEN fig_st.ps_file_names=ptr_new(ps_file_names)

RETURN,fig_st

END
