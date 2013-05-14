
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
    list = new LinkedList(new ListItem(0,null,null));
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
      list.insertAfter(System.rand()%1000,it);
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
	int elem; elem = it.nextElement();
        System.print(elem); System.print(" ");
    }
    System.println("");
  }

  void checkList(LinkedList list)
  {
    if(this.checkListAux(list.head,list.head))
      System.println("Order is correct");
    else System.println("Order is NOT correct");
  }

  bool checkListAux(ListItem head,ListItem current)
  {
    if(head==current) return true;
    if(current.next == null) return true;
    int elem; elem = current.data;
    int nextelem; elem = current.next.data;

    if(elem<nextelem)
      return this.checkListAux(head,current.next);
    else return false;
  }


  LinkedList sort(LinkedList list)
  {
    LinkedList sorted;
    sorted = this.newList();
    ListIterator lit;
    lit = list.head();
    lit.next();

    while(lit.hasMoreElements())
    {
	int elem; elem = lit.nextElement();
        bool notbreak; notbreak = true;
	ListIterator sit;
	sit = sorted.head();
        sit.next();

	while(sit.hasMoreElements() && notbreak)
	{
	    int cur; cur = sit.nextElement();

            if(elem<cur)
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

class ListItem {
  int data;
  ListItem previous;
  ListItem  next;

}

class ListIterator {
  LinkedList owner;
  ListItem pos;

  void head() { this.pos = this.owner.head; }
  void next() { 
    this.pos = this.pos.next; 
  }

  void previous() { this.pos = this.pos.previous; }

  bool hasMoreElements() {  
    return this.pos != this.owner.head;
  }

  int nextElement() {
    if (this.pos != this.owner.head) {
       int data; data  = this.pos.data;
       this.next();
       return data;
    }
    return 0;
  }
}

class LinkedList {
  ListItem head;

  void clear() {
      this.head.next = this.head;
      this.head.previous = this.head;
  }

  bool isEmpty() { return this.head.next == this.head; }

  void insertAfter(int data, ListIterator cursor) {
    ListItem newItem;
    newItem = new ListItem(data,cursor.pos,cursor.pos.next);
    if(newItem == null) System.println("newItem is null");
    if(newItem.next == null) System.println("newItem.next is null");
    newItem.next.previous = newItem;
    cursor.pos.next = newItem;
  }

  void insertBefore(int data, ListIterator cursor) {
    ListItem newItem;
    newItem = new ListItem(data, cursor.pos.previous, cursor.pos);
    newItem.previous.next = newItem;
    cursor.pos.previous = newItem;
  }

  ListIterator head() {
    return new ListIterator(this, this.head);
  }

  ListIterator find(int data) {
    if (this.isEmpty()) return null;

    ListItem pos;
    pos = this.head;

    while (pos.next != this.head) {  // There are still elements to be inspected
      pos = pos.next;
      if (pos.data == data) return new ListIterator(this, pos);
    }
    return null;
  }

}
