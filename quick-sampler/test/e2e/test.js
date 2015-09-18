'use strict';

/* http://docs.angularjs.org/guide/dev_guide.e2e-testing */

describe('Quick Sampler App', function() {

  function checkNavCss(buttonText, buttonIndex) {
      var expected, item;
      element.all(by.css('.nav li')).then(function(items) {
          expect(items[buttonIndex].getText()).toBe(buttonText);
          for (var i = 0; i < items.length; i++) {
              expected = (i === buttonIndex) ? 'active' : '';
              item = items[i];
              expect(item.getAttribute('class')).toBe(expected);
          }
      });
  }

  describe('home page', function() {

    beforeEach(function(){
        browser.get('#');
    });

    it('should have the correct title', function() {
      var title = browser.getTitle();
      expect(title).toEqual('Quick Sampler');
    });

    it('should highlight the correct nav buttons', function() {
        checkNavCss('Home', 0);
    });

    it('should preserve form values if navigating to about page and back', function() {
      element(by.id('id_seed')).sendKeys('abc');
      element(by.linkText('About')).click();
      expect(element(by.tagName('h1')).getText()).toEqual('About');
      element(by.linkText('Home')).click();
      expect(element(by.id('id_seed')).getAttribute('value')).toEqual('abc');
    });

    it('should display no error for the random seed if an ASCII seed is typed', function() {
      element(by.id('id_seed')).sendKeys('abc123');
      element(by.id('submit')).click();
      expect(element(by.id('id_seed_error')).getText())
        .toEqual('');
    });

    it('should display an error if no random seed is typed', function() {
      element(by.id('submit')).click();
      expect(element(by.id('id_seed_error')).getText())
        .toEqual('A random seed is required.');
    });

    // Skip this test if Chrome is being used because the ChromeDriver
    // does not currently support sending non-BMP characters:
    // https://code.google.com/p/chromedriver/issues/detail?id=187
    (browser.browserName === 'chrome' ? xit : it)(
      'should display an error if a random seed is typed with a non-BMP character',
        function() {
          element(by.id('id_seed')).sendKeys('abc123\ud83d\ude00');
          element(by.id('submit')).click();
          expect(element(by.id('id_seed_error')).getText())
            .toEqual('Random seeds with non-basic characters are not yet supported.');
    });

    it('should update the highest item input if only the total items is updated', function() {
      expect(element(by.id('id_highest_item')).getAttribute('value')).toBe('');
      element(by.id('id_total_count')).sendKeys('100');
      // Sanity check that the total count value registers the change.
      // We do this because an old Selenium version had a bug that caused
      // this assertion to fail.
      expect(element(by.id('id_total_count')).getAttribute('value')).toBe('100');
      expect(element(by.id('id_highest_item')).getAttribute('value')).toBe('100');
    });

  });

  describe('about page', function() {

    beforeEach(function(){
        browser.get('#/about');
    });

    it('should highlight the correct nav buttons', function() {
        checkNavCss('About', 1);
    });

  });

});
