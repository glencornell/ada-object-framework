with Objects;
with Widgets;
with Labels;

package My_Objects is
   
   Top               : aliased Widgets.Widget;
   Form              : aliased Widgets.Widget;
   Label             : aliased Labels.Label;
   Row_Column_Layout : aliased Widgets.Widget;
   Ok                : aliased Widgets.Widget;
   Cancel            : aliased Widgets.Widget;
   
   Top_Ptr               : Objects.Object_Ptr := Top'Access;
   Form_Ptr              : Objects.Object_Ptr := Form'Access;
   Label_Ptr             : Objects.Object_Ptr := Label'Access;
   Row_Column_Layout_Ptr : Objects.Object_Ptr := Row_Column_Layout'Access;
   Ok_Ptr                : Objects.Object_Ptr := Ok'Access;
   Cancel_Ptr            : Objects.Object_Ptr := Cancel'Access;
   
end My_Objects;
