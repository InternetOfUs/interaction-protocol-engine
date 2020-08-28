/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.persistence;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.function.Consumer;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.common.TimeManager;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.PublishedNorm;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.PublishedNormTest;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;

/**
 * Integration test over the {@link NormsRepository}.
 *
 * @see NormsRepository
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class NormsRepositoryIT {

  /**
   * Verify that can not found a published norm if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#searchPublishedNorm(String, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotFoundUndefinedPublishedNorm(final Vertx vertx, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).searchPublishedNorm("undefined norm identifier", testContext.failing(failed -> {
      testContext.completeNow();
    }));

  }

  /**
   * Verify that can not found a published norm object if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#searchPublishedNormObject(String, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotFoundUndefinedPublishedNormObject(final Vertx vertx, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).searchPublishedNormObject("undefined norm identifier", testContext.failing(failed -> {
      testContext.completeNow();
    }));

  }

  /**
   * Verify that can found a published norm.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#searchPublishedNorm(String, io.vertx.core.Handler)
   */
  @Test
  public void shouldFoundPublishedNorm(final Vertx vertx, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).storePublishedNorm(new PublishedNorm(), testContext.succeeding(storedNorm -> {

      NormsRepository.createProxy(vertx).searchPublishedNorm(storedNorm.id, testContext.succeeding(foundNorm -> testContext.verify(() -> {
        assertThat(foundNorm).isEqualTo(storedNorm);
        testContext.completeNow();
      })));

    }));

  }

  /**
   * Verify that can found a published norm object.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#searchPublishedNormObject(String, io.vertx.core.Handler)
   */
  @Test
  public void shouldFoundPublishedNormObject(final Vertx vertx, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).storePublishedNorm(new JsonObject(), testContext.succeeding(storedNorm -> {

      NormsRepository.createProxy(vertx).searchPublishedNormObject(storedNorm.getString("id"), testContext.succeeding(foundNorm -> testContext.verify(() -> {
        assertThat(foundNorm).isEqualTo(storedNorm);
        testContext.completeNow();
      })));

    }));

  }

  /**
   * Verify that can not store a published norm that can not be an object.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#updatePublishedNorm(PublishedNorm, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotStoreANormThatCanNotBeAnObject(final Vertx vertx, final VertxTestContext testContext) {

    final PublishedNorm norm = new PublishedNorm() {

      /**
       * {@inheritDoc}
       */
      @Override
      public JsonObject toJsonObject() {

        return null;
      }
    };
    norm.id = "undefined norm identifier";
    NormsRepository.createProxy(vertx).storePublishedNorm(norm, testContext.failing(failed -> {
      testContext.completeNow();
    }));

  }

  /**
   * Verify that can store a published norm.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#searchPublishedNorm(String, io.vertx.core.Handler)
   */
  @Test
  public void shouldStorePublishedNorm(final Vertx vertx, final VertxTestContext testContext) {

    final var norm = new PublishedNorm();
    final var now = TimeManager.now();
    NormsRepository.createProxy(vertx).storePublishedNorm(norm, testContext.succeeding(storedNorm -> testContext.verify(() -> {

      assertThat(storedNorm).isNotNull();
      assertThat(storedNorm.id).isNotEmpty();
      assertThat(storedNorm._creationTs).isGreaterThanOrEqualTo(now);
      assertThat(storedNorm._lastUpdateTs).isGreaterThanOrEqualTo(now);
      testContext.completeNow();
    })));

  }

  /**
   * Verify that can store a published norm object.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#storePublishedNorm(JsonObject, io.vertx.core.Handler)
   */
  @Test
  public void shouldStorePublishedNormObject(final Vertx vertx, final VertxTestContext testContext) {

    final var now = TimeManager.now();
    NormsRepository.createProxy(vertx).storePublishedNorm(new JsonObject(), testContext.succeeding(storedNorm -> testContext.verify(() -> {

      assertThat(storedNorm).isNotNull();
      final var id = storedNorm.getString("id");
      assertThat(id).isNotEmpty();
      assertThat(storedNorm.getLong("_creationTs", 0l)).isNotEqualTo(0).isGreaterThanOrEqualTo(now);
      assertThat(storedNorm.getLong("_lastUpdateTs", 0l)).isNotEqualTo(0).isGreaterThanOrEqualTo(now);
      testContext.completeNow();
    })));

  }

  /**
   * Verify that can not update a published norm if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#updatePublishedNorm(PublishedNorm, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotUpdateUndefinedPublishedNorm(final Vertx vertx, final VertxTestContext testContext) {

    final var norm = new PublishedNorm();
    norm.id = "undefined norm identifier";
    NormsRepository.createProxy(vertx).updatePublishedNorm(norm, testContext.failing(failed -> {
      testContext.completeNow();
    }));

  }

  /**
   * Verify that can not update a published norm if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotUpdateUndefinedPublishedNormObject(final Vertx vertx, final VertxTestContext testContext) {

    final var norm = new JsonObject().put("id", "undefined norm identifier");
    NormsRepository.createProxy(vertx).updatePublishedNorm(norm, testContext.failing(failed -> {
      testContext.completeNow();
    }));

  }

  /**
   * Verify that can not update a published norm if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#updatePublishedNorm(PublishedNorm, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotUpdateANormThatCanNotBeAnObject(final Vertx vertx, final VertxTestContext testContext) {

    final PublishedNorm norm = new PublishedNorm() {

      /**
       * {@inheritDoc}
       */
      @Override
      public JsonObject toJsonObject() {

        return null;
      }
    };
    norm.id = "undefined norm identifier";
    NormsRepository.createProxy(vertx).updatePublishedNorm(norm, testContext.failing(failed -> {
      testContext.completeNow();
    }));

  }

  /**
   * Verify that can update a published norm.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#updatePublishedNorm(PublishedNorm, io.vertx.core.Handler)
   */
  @Test
  public void shouldUpdatePublishedNorm(final Vertx vertx, final VertxTestContext testContext) {

    final var norm = new PublishedNorm();

    NormsRepository.createProxy(vertx).storePublishedNorm(norm, testContext.succeeding(stored -> testContext.verify(() -> {

      final var update = new PublishedNormTest().createModelExample(23);
      update.id = stored.id;
      Thread.sleep(1000);
      final var now = TimeManager.now();
      NormsRepository.createProxy(vertx).updatePublishedNorm(update, testContext.succeeding(empty -> {

        NormsRepository.createProxy(vertx).searchPublishedNorm(stored.id, testContext.succeeding(foundNorm -> testContext.verify(() -> {

          assertThat(foundNorm._creationTs).isEqualTo(stored._creationTs);
          assertThat(foundNorm._lastUpdateTs).isGreaterThanOrEqualTo(now);
          update._creationTs = stored._creationTs;
          update._lastUpdateTs = foundNorm._lastUpdateTs;
          assertThat(foundNorm).isEqualTo(update);
          testContext.completeNow();
        })));
      }));

    })));

  }

  /**
   * Verify that update a defined published norm object.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
   */
  @Test
  public void shouldUpdatePublishedNormObject(final Vertx vertx, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).storePublishedNorm(new JsonObject().put("name", "Norm name"), testContext.succeeding(stored -> testContext.verify(() -> {

      final var id = stored.getString("id");
      final var update = new JsonObject().put("id", id).put("name", "New norm name");
      NormsRepository.createProxy(vertx).updatePublishedNorm(update, testContext.succeeding(empty -> testContext.verify(() -> {

        NormsRepository.createProxy(vertx).searchPublishedNormObject(id, testContext.succeeding(foundNorm -> testContext.verify(() -> {
          stored.put("name", "New norm name");
          assertThat(foundNorm).isEqualTo(stored);
          testContext.completeNow();
        })));
      })));

    })));

  }

  /**
   * Verify that can not delete a published norm if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#searchPublishedNorm(String, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotDeleteUndefinedPublishedNorm(final Vertx vertx, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).deletePublishedNorm("undefined norm identifier", testContext.failing(failed -> {
      testContext.completeNow();
    }));

  }

  /**
   * Verify that can delete a published norm.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
   */
  @Test
  public void shouldDeletePublishedNorm(final Vertx vertx, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).storePublishedNorm(new JsonObject(), testContext.succeeding(stored -> {

      final var id = stored.getString("id");
      NormsRepository.createProxy(vertx).deletePublishedNorm(id, testContext.succeeding(success -> {

        NormsRepository.createProxy(vertx).searchPublishedNormObject(id, testContext.failing(search -> {

          testContext.completeNow();

        }));

      }));

    }));

  }

  /**
   *
   * Create some {@link PublishedNormTest#createModelExample(int)}.
   *
   * @param vertx           event bus to use.
   * @param testContext     context that executes the test.
   * @param change          function to modify the pattern before to store it.
   * @param max             number maximum of norms to create.
   * @param norms           list to add the created norms.
   * @param creationHandler that manage the creation.
   */
  public void storeSomePublishedNorms(final Vertx vertx, final VertxTestContext testContext, final Consumer<PublishedNorm> change, final int max, final List<PublishedNorm> norms, final Handler<AsyncResult<Void>> creationHandler) {

    if (norms.size() == max) {

      creationHandler.handle(Future.succeededFuture());

    } else {

      final var norm = new PublishedNormTest().createModelExample(norms.size());
      change.accept(norm);
      NormsRepository.createProxy(vertx).storePublishedNorm(norm, testContext.succeeding(stored -> {

        norms.add(stored);
        this.storeSomePublishedNorms(vertx, testContext, change, max, norms, creationHandler);
      }));
    }

  }

  /**
   * Verify that can obtain the published norms by name.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#retrievePublishedNormsPageObject(JsonObject, JsonObject, int, int, io.vertx.core.Handler)
   */
  @Test
  public void shoulFoundPublishedNormPageByName(final Vertx vertx, final VertxTestContext testContext) {

    final var repository = NormsRepository.createProxy(vertx);
    final var name = UUID.randomUUID().toString();
    final var query = NormsRepository.createPublishedNormsPageQuery(name, null, null, null, null, null);
    final var sort = new JsonObject();
    repository.retrievePublishedNormsPage(query, sort, 0, 10, testContext.succeeding(found -> testContext.verify(() -> {

      assertThat(found).isNotNull();
      assertThat(found.total).isEqualTo(0l);
      assertThat(found.norms).isNull();
      final List<PublishedNorm> norms = new ArrayList<>();
      this.storeSomePublishedNorms(vertx, testContext, norm -> norm.name = name, 20, norms, testContext.succeeding(empty -> {

        repository.retrievePublishedNormsPage(query, sort, 0, 10, testContext.succeeding(found2 -> testContext.verify(() -> {

          assertThat(found2).isNotNull();
          assertThat(found2.total).isEqualTo(20l);
          assertThat(found2.norms).isEqualTo(norms.subList(0, 10));

          repository.retrievePublishedNormsPage(query, sort, 5, 6, testContext.succeeding(found3 -> testContext.verify(() -> {

            assertThat(found3).isNotNull();
            assertThat(found3.total).isEqualTo(20l);
            assertThat(found3.norms).isEqualTo(norms.subList(5, 11));
            testContext.completeNow();

          })));

        })));

      }));

    })));

  }

  /**
   * Verify that can obtain the published norms by description.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#retrievePublishedNormsPageObject(JsonObject, JsonObject, int, int, io.vertx.core.Handler)
   */
  @Test
  public void shoulFoundPublishedNormPageByDescription(final Vertx vertx, final VertxTestContext testContext) {

    final var repository = NormsRepository.createProxy(vertx);
    final var description = UUID.randomUUID().toString();
    final var query = NormsRepository.createPublishedNormsPageQuery(null, description, null, null, null, null);
    final var sort = new JsonObject();
    repository.retrievePublishedNormsPage(query, sort, 0, 10, testContext.succeeding(found -> testContext.verify(() -> {

      assertThat(found).isNotNull();
      assertThat(found.total).isEqualTo(0l);
      assertThat(found.norms).isNull();
      final List<PublishedNorm> norms = new ArrayList<>();
      this.storeSomePublishedNorms(vertx, testContext, norm -> norm.description = description, 20, norms, testContext.succeeding(empty -> {

        repository.retrievePublishedNormsPage(query, sort, 0, 10, testContext.succeeding(found2 -> testContext.verify(() -> {

          assertThat(found2).isNotNull();
          assertThat(found2.total).isEqualTo(20l);
          assertThat(found2.norms).isEqualTo(norms.subList(0, 10));

          repository.retrievePublishedNormsPage(query, sort, 5, 6, testContext.succeeding(found3 -> testContext.verify(() -> {

            assertThat(found3).isNotNull();
            assertThat(found3.total).isEqualTo(20l);
            assertThat(found3.norms).isEqualTo(norms.subList(5, 11));
            testContext.completeNow();

          })));

        })));

      }));

    })));

  }

}
