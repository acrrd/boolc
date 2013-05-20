
class Main
{
  int main()
  {
    System.srand();
    LinkedList list;
    list = this.newList();
    int num_elems; 
    num_elems = 10;
    this.fillList(num_elems,list);
    this.printList(list);
    LinkedList sorted;
    sorted = this.sort(list);
    this.checkList(sorted);
    this.printList(sorted);
  }

  LinkedList newList() { 
    LinkedList list;
    list = new LinkedList(new ListItem(null,null,null));
    list.clear();
    return list;
  }

  void fillList(int num_elems, LinkedList list)
  {
    ListIterator it;
    it = list.head();
    int i; i = 0;
    while(i<num_elems)
    {
      i=i+1;
      list.insertAfter(new Integer(System.rand()%1000),it);
      it.next();
    }
  }

  void printList(LinkedList list)
  {
    ListIterator it; 
    it = list.head();
    it.next();

    while(it.hasMoreElements())
    {
	int elem; elem = this.getValue(it.nextElement());
        System.print(elem); System.print(" ");
    }
    System.println("");
  }

  void checkList(LinkedList list)
  {
    if(this.checkListAux(list.head,list.head.next))
      System.println("Order is correct");
    else System.println("Order is NOT correct");
  }

  bool checkListAux(ListItem head,ListItem current)
  {
    ListItem next; next = current.next;
    if(head==next) return true;
    int elem; elem = this.getValue(current.data);
    int nextelem; nextelem = this.getValue(next.data);
    if(elem<=nextelem)
      return this.checkListAux(head,next);
    else return false;
  }

  int getValue(Object o){ return ((Integer)o).value; }

  LinkedList sort(LinkedList list)
  {
    LinkedList sorted;
    sorted = this.newList();
    ListIterator lit;
    lit = list.head();
    lit.next();

    while(lit.hasMoreElements())
    {
	Integer elem; elem = (Integer)lit.nextElement();
        bool notbreak; notbreak = true;
	ListIterator sit;
	sit = sorted.head();
        sit.next();
	while(sit.hasMoreElements() && notbreak)
	{
	    Integer cur; cur = (Integer)sit.nextElement();

            if(elem.value<cur.value)
            {
		sit.previous();
		sorted.insertBefore(elem,sit);
                notbreak = false;
            }
        }
        if(notbreak){
          sorted.insertBefore(elem,sit);
        }
    }
    return sorted;
  }
}

class Integer { int value; }

class ListItem {
  Object data;
  ListItem previous;
  ListItem  next;

}

class ListIterator {
  LinkedList owner;
  ListItem pos;

  void head() { this.pos = this.owner.head; }
  void next() { this.pos = this.pos.next; }

  void previous() { this.pos = this.pos.previous; }

  bool hasMoreElements() {  
    return this.pos != this.owner.head;
  }

  Object nextElement() {
    if (this.pos != this.owner.head) {
       Object data; data  = this.pos.data;
       this.next();
       return data;
    }
    return null;
  }
}

class LinkedList {
  ListItem head;

  void clear() {
      this.head.next = this.head;
      this.head.previous = this.head;
  }

  bool isEmpty() { return this.head.next == this.head; }

  void insertAfter(Object data, ListIterator cursor) {
    ListItem newItem;
    newItem = new ListItem(data,cursor.pos,cursor.pos.next);
    newItem.next.previous = newItem;
    cursor.pos.next = newItem;
  }

  void insertBefore(Object data, ListIterator cursor) {
    ListItem newItem;
    newItem = new ListItem(data, cursor.pos.previous, cursor.pos);
    newItem.previous.next = newItem;
    cursor.pos.previous = newItem;
  }

  ListIterator head() {
    return new ListIterator(this, this.head);
  }
}
